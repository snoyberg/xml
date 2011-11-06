{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Text.HTML.TagStream.Parser where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.Char8
import Blaze.ByteString.Builder (toByteString)
import Text.HTML.TagStream.Types
import Text.HTML.TagStream.Utils (cons, append)

{--
 - match quoted string, can fail.
 -}
quoted :: Char -> Parser ByteString
quoted q = append <$> takeTill (in2 ('\\',q))
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
             cons <$> satisfy (/='>')
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
  where comment' = append <$> takeTill (=='-')
                          <*> ( string "-->" *> return ""
                                <|> atLeast 1 comment' )

{--
 - tags begine with <! , e.g. <!DOCTYPE ...>
 -}
special :: Parser Token
special = Special
          <$> ( cons <$> satisfy (not . ((=='-') ||. isSpace))
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
incomplete = Incomplete . cons '<' <$> takeByteString

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
              <|> (:[]) . Incomplete . append script <$> takeByteString
  where
    script = toByteString $ showToken id t
    surround q s = S.concat [S.singleton q, s, S.singleton q]
    loop acc = do
        s <- takeTill (in3 ('<','\'','"'))
        let acc' = if S.null s then acc else Text s:acc
        mq <- maybeP (satisfy (in2 ('"','\'')))
        case mq of
            Just q -> Text . surround q <$> quoted q >>=
                      loop . (:acc')
            Nothing -> (:acc') <$> scriptEnd

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
atLeast n p = cons <$> anyChar <*> atLeast (n-1) p

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
