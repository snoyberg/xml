{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}
module Text.HTML.TagStream.ByteString where

import Control.Applicative
import Control.Monad (unless)

import Data.Monoid (mconcat)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.Char8
import Data.Conduit (GInfConduit, awaitE, yield)

import qualified Blaze.ByteString.Builder as B
import Text.HTML.TagStream.Types
import Text.HTML.TagStream.Utils (splitAccum)

type Token = Token' ByteString
type Attr = Attr' ByteString

{--
 - match quoted string, can fail.
 -}
quoted :: Char -> Parser ByteString
quoted q = S.append <$> takeTill (in2 ('\\',q))
                  <*> ( char q *> pure ""
                        <|> char '\\' *> atLeast 1 (quoted q) )

quotedOr :: Parser ByteString -> Parser ByteString
quotedOr p = maybeP (satisfy (in2 ('"','\''))) >>=
             maybe p quoted

{--
 - attribute value, can't fail.
 -}
attrValue :: Parser ByteString
attrValue = quotedOr $ takeTill ((=='>') ||. isSpace)

{--
 - attribute name, at least one char, can fail when meet tag end.
 - might match self-close tag end "/>" , make sure match `tagEnd' first.
 -}
attrName :: Parser ByteString
attrName = quotedOr $
             S.cons <$> satisfy (/='>')
                    <*> takeTill (in3 ('/','>','=') ||. isSpace)

{--
 - tag end, return self-close or not, can fail.
 -}
tagEnd :: Parser Bool
tagEnd = char '>' *> pure False
     <|> string "/>" *> pure True

{--
 - attribute pair or tag end, can fail if tag end met.
 -}
attr :: Parser Attr
attr = (,) <$> attrName <* skipSpace
           <*> ( boolP (char '=') >>=
                 cond (skipSpace *> attrValue)
                      (pure "")
               )

{--
 - all attributes before tag end. can't fail.
 -}
attrs :: Parser ([Attr], Bool)
attrs = loop []
  where
    loop acc = skipSpace *> (Left <$> tagEnd <|> Right <$> attr) >>=
               either
                 (return . (reverse acc,))
                 (loop . (:acc))

{--
 - comment tag without prefix.
 -}
comment :: Parser Token
comment = Comment <$> comment'
  where comment' = S.append <$> takeTill (=='-')
                          <*> ( string "-->" *> return ""
                                <|> atLeast 1 comment' )

{--
 - tags begine with <! , e.g. <!DOCTYPE ...>
 -}
special :: Parser Token
special = Special
          <$> ( S.cons <$> satisfy (not . ((=='-') ||. isSpace))
                     <*> takeTill ((=='>') ||. isSpace)
                     <* skipSpace )
          <*> takeTill (=='>') <* char '>'

{--
 - parse a tag, can fail.
 -}
tag :: Parser Token
tag = do
    t <- string "/"     *> return TagTypeClose
         <|> string "!" *> return TagTypeSpecial
         <|> return TagTypeNormal
    case t of
        TagTypeClose ->
            TagClose <$> takeTill (=='>')
            <* char '>'
        TagTypeSpecial -> boolP (string "--") >>=
                          cond comment special
        TagTypeNormal -> do
            name <- takeTill (in3 ('<','>','/') ||. isSpace)
            (as, close) <- attrs
            skipSpace
            return $ TagOpen name as close

{--
 - record incomplete tag for streamline processing.
 -}
incomplete :: Parser Token
incomplete = Incomplete . S.cons '<' <$> takeByteString

{--
 - parse text node. consume at least one char, to make sure progress.
 -}
text :: Parser Token
text = Text <$> atLeast 1 (takeTill (=='<'))

token :: Parser Token
token = char '<' *> (tag <|> incomplete)
        <|> text

{--
 - treat script tag specially, can't fail.
 -}
tillScriptEnd :: Token -> Parser [Token]
tillScriptEnd t = reverse <$> loop [t]
              <|> (:[]) . Incomplete . S.append script <$> takeByteString
  where
    script = B.toByteString $ showToken id t
    loop acc = (:acc) <$> scriptEnd
           <|> (text >>= loop . (:acc))
    scriptEnd = string "</script>" *> return (TagClose "script")

html :: Parser [Token]
html = tokens <|> pure []
  where
    tokens :: Parser [Token]
    tokens = do
        t <- token
        case t of
            (TagOpen name _ close)
              | not close && name=="script"
                -> (++) <$> tillScriptEnd t <*> html
            _ -> (t:) <$> html

decode :: ByteString -> Either String [Token]
decode = parseOnly html

{--
 - Utils {{{
 -}

atLeast :: Int -> Parser ByteString -> Parser ByteString
atLeast 0 p = p
atLeast n p = S.cons <$> anyChar <*> atLeast (n-1) p

cond :: a -> a -> Bool -> a
cond a1 a2 b = if b then a1 else a2

(||.) :: Applicative f => f Bool -> f Bool -> f Bool
(||.) = liftA2 (||)

in2 :: Eq a => (a,a) -> a -> Bool
in2 (a1,a2) a = a==a1 || a==a2

in3 :: Eq a => (a,a,a) -> a -> Bool
in3 (a1,a2,a3) a = a==a1 || a==a2 || a==a3

boolP :: Parser a -> Parser Bool
boolP p = p *> pure True <|> pure False

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = Just <$> p <|> return Nothing
-- }}}

-- {{{ encode tokens
cc :: [ByteString] -> B.Builder
cc = mconcat . map B.fromByteString

showToken :: (ByteString -> ByteString) -> Token -> B.Builder
showToken hl (TagOpen name as close) =
    cc $ [hl "<", name]
      ++ map showAttr as
      ++ [hl (if close then "/>" else ">")]
  where
    showAttr :: Attr -> ByteString
    showAttr (key, value) = S.concat $ [" ", key, hl "=\""] ++ map escape (S.unpack value) ++ [hl "\""]
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = S.singleton c
showToken hl (TagClose name) = cc [hl "</", name, hl ">"]
showToken _ (Text s) = B.fromByteString s
showToken hl (Comment s) = cc [hl "<!--", s, hl "-->"]
showToken hl (Special name s) = cc [hl "<!", name, " ", s, hl ">"]
showToken _ (Incomplete s) = B.fromByteString s
-- }}}

-- {{{ Stream
tokenStream :: Monad m => GInfConduit ByteString m Token
tokenStream =
    loop S.empty
  where
    loop accum = awaitE >>= either (close accum) (push accum)

    push accum input =
        case parseOnly html (accum `S.append` input) of
            Right (splitAccum -> (accum', tokens)) -> mapM_ yield tokens >> loop accum'
            Left err -> fail err

    close s r = do
        unless (S.null s) $ yield $ Text s
        return r

-- }}}
