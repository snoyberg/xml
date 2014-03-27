{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Text.HTML.TagStream.Text where

import           Control.Applicative
import           Control.Monad (unless, when, liftM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Data.Char
import qualified Data.Conduit.List as CL
import           Data.Default
import           Prelude hiding (mapM)

import qualified Data.Attoparsec.ByteString.Char8 as S
import           Data.Attoparsec.Text
import           Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import           Data.Conduit
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mconcat)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import           Data.Traversable (mapM)
import qualified Text.XML.Stream.Parse as XML
#if MIN_VERSION_conduit(1, 0, 0)
#else
import           Data.Conduit.Internal (pipeL)
#endif
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Text as C

import qualified Text.HTML.TagStream.ByteString as S
import           Text.HTML.TagStream.Entities
import           Text.HTML.TagStream.Types
import           Text.HTML.TagStream.Utils (splitAccum)

type Token = Token' Text
type Attr = Attr' Text

{--
 - match quoted string, can fail.
 -}
quoted :: Char -> Parser Text
quoted q = T.append <$> takeTill (in2 ('\\',q))
                    <*> ( char q *> pure ""
                      <|> char '\\' *> atLeast 1 (quoted q) )

quotedOr :: Parser Text -> Parser Text
quotedOr p = maybeP (satisfy (in2 ('"','\''))) >>=
             maybe p quoted

{--
 - attribute value, can't fail.
 -}
attrValue :: Parser Text
attrValue = quotedOr $ takeTill ((=='>') ||. isSpace)

{--
 - attribute name, at least one char, can fail when meet tag end.
 - might match self-close tag end "/>" , make sure match `tagEnd' first.
 -}
attrName :: Parser Text
attrName = quotedOr $
             T.cons <$> satisfy (/='>')
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
  where comment' = T.append <$> takeTill (=='-')
                            <*> ( string "-->" *> return ""
                              <|> atLeast 1 comment' )

{--
 - tags begine with <! , e.g. <!DOCTYPE ...>
 -}
special :: Parser Token
special = Special
          <$> ( T.cons <$> satisfy (not . ((=='-') ||. isSpace))
                       <*> takeTill ((=='>') ||. isSpace)
                       <* skipSpace )
          <*> takeTill (=='>') <* char '>'

{--
 - parse a tag, can fail.
 -}
tag :: Parser Token
tag = do
    t <-     string "/" *> return TagTypeClose
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
            return $ TagOpen name as close

{--
 - record incomplete tag for streamline processing.
 -}
incomplete :: Parser Token
incomplete = Incomplete . T.cons '<' <$> takeText

{--
 - parse text node. consume at least one char, to make sure progress.
 -}
text :: Parser Token
text = Text <$> atLeast 1 (takeTill (=='<'))

-- | Decode the HTML entities e.g. @&amp;@ in some text into @&@.
decodeEntitiesText :: Monad m => Conduit Token m Token
decodeEntitiesText =
  decodeEntities
    Dec { decToS     = L.toStrict . B.toLazyText
        , decBreak   = T.break
        , decBuilder = B.fromText
        , decDrop    = T.drop
        , decEntity  = decodeEntity
        , decUncons  = T.uncons }
  where decodeEntity entity =
          CL.sourceList ["&",entity,";"]
          $= XML.parseText def { XML.psDecodeEntities = XML.decodeHtmlEntities }
          $= CL.map snd
          $$ XML.content

token :: Parser Token
token = char '<' *> (tag <|> incomplete)
    <|> text

{--
 - treat script tag specially, can't fail.
 -}
tillScriptEnd :: Token -> Parser [Token]
tillScriptEnd t = reverse <$> loop [t]
              <|> (:[]) . Incomplete . T.append script <$> takeText
  where
    script = L.toStrict . B.toLazyText $ showToken id t
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

decode :: Text -> Either String [Token]
decode = fmap decodeEntitiesText' . parseOnly html
  where
    decodeEntitiesText' tokens = runIdentity $ mapM_ yield tokens $$ decodeEntitiesText =$ CL.consume

{--
 - Utils {{{
 -}

atLeast :: Int -> Parser Text -> Parser Text
atLeast 0 p = p
atLeast n p = T.cons <$> anyChar <*> atLeast (n-1) p

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
cc :: [Text] -> B.Builder
cc = mconcat . map B.fromText

showToken :: (Text -> Text) -> Token -> B.Builder
showToken hl (TagOpen name as close) =
    cc $ [hl "<", name]
      ++ map showAttr as
      ++ [hl (if close then "/>" else ">")]
  where
    showAttr :: Attr -> Text
    showAttr (key, value) = T.concat $ [" ", key, hl "=\""] ++ map escape (T.unpack value) ++ [hl "\""]
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = T.singleton c
showToken hl (TagClose name) = cc [hl "</", name, hl ">"]
showToken _ (Text s) = B.fromText s
showToken hl (Comment s) = cc [hl "<!--", s, hl "-->"]
showToken hl (Special name s) = cc [hl "<!", name, " ", s, hl ">"]
showToken _ (Incomplete s) = B.fromText s
-- }}}

-- {{{ Stream
tokenStream :: Monad m
#if MIN_VERSION_conduit(1, 0, 0)
            => Conduit Text m Token
#else
            => GInfConduit Text m Token
#endif
tokenStream =
    loop T.empty =$= decodeEntitiesText
  where
#if MIN_VERSION_conduit(1, 0, 0)
    loop accum = await >>= maybe (close accum ()) (push accum)
#else
    loop accum = awaitE >>= either (close accum) (push accum)
#endif

    push accum input =
        case parseOnly html (accum `T.append` input) of
            Right (splitAccum -> (accum', tokens)) -> mapM_ yield tokens >> loop accum'
            Left err -> fail err

    close s r = do
        unless (T.null s) $ yield $ Text s
        return r

-- | like `tokenStream', but it process `ByteString' input, decode it according to xml version tag.
--
-- Only support utf-8 and iso8859 for now.
tokenStreamBS :: MonadThrow m
#if MIN_VERSION_conduit(1, 0, 0)
              => Conduit ByteString m Token
#else
              => GLInfConduit ByteString m Token
#endif
tokenStreamBS = do
    -- try to peek the first tag to find the xml encoding.
    tk <- C.sinkParser (skipBOM *> S.skipSpace *> S.char '<' *> S.tag)

    let (mencoding, yieldToken) =
          case tk of
            (TagOpen "?xml" as _) ->
                (lookup "encoding" as, False)
            _ -> (Nothing, True)

    let codec = fromMaybe C.utf8 (mencoding >>= getCodec . CI.mk)

    when yieldToken $ lift (mapM (decodeBS codec) tk) >>= yield

#if MIN_VERSION_conduit(1, 0, 0)
    C.decode codec =$= tokenStream
#else
    C.decode codec `pipeL` tokenStream
#endif
  where
    skipBOM :: S.Parser ()
    skipBOM =
        ( S.string "\xff\xfe"
          <|> S.string "\xef\xbb\xbf"
        ) *> return ()
        <|> return ()

    getCodec :: CI.CI ByteString -> Maybe C.Codec
    getCodec c =
        case c of
            "utf-8"   -> Just C.utf8
            "utf8"    -> Just C.utf8
            "iso8859" -> Just C.iso8859_1
            _         -> Nothing

    --decodeBS :: C.Codec -> ByteString -> m Text
    decodeBS codec bs = liftM T.concat $ yield bs $= C.decode codec $$ C.consume
-- }}}
