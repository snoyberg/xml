{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Text.HTML.TagStream.Text
  ( Token (..)
  , tokenStream
  ) where

import           Control.Applicative
import           Control.Monad (unless, when, liftM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Data.Char
import qualified Data.Conduit.List as CL
import           Prelude hiding (mapM)

import qualified Data.Attoparsec.ByteString.Char8 as S
import           Data.Attoparsec.Text
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid (mconcat, (<>))
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
import           Data.String (IsString)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Control.Arrow (first, second)

data Token
  = TagOpen Text (Map Text Text) Bool
  | TagClose Text
  | Text Text
  | Comment Text
  | Special Text Text
  | Incomplete Text
  deriving (Eq, Show)

data TagType
  = TagTypeClose
  | TagTypeSpecial
  | TagTypeNormal

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
attr :: Parser (Text, Text)
attr = (,) <$> attrName <* skipSpace
           <*> ( boolP (char '=') >>=
                 cond (skipSpace *> attrValue)
                      (pure "")
               )

{--
 - all attributes before tag end. can't fail.
 -}
attrs :: Parser (Map Text Text, Bool)
attrs = loop Map.empty
  where
    loop acc = skipSpace *> (Left <$> tagEnd <|> Right <$> attr) >>=
               either
                 (return . (acc,))
                 (\(key, value) -> loop $ Map.insert key value acc)

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
          $= XML.parseText XML.def { XML.psDecodeEntities = XML.decodeHtmlEntities }
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
cc :: [Text] -> B.Builder -- FIXME use Builder correctly
cc = mconcat . map B.fromText

showToken :: (Text -> Text) -> Token -> B.Builder
showToken hl (TagOpen name as close) =
    cc $ [hl "<", name]
      ++ map showAttr (Map.toList as)
      ++ [hl (if close then "/>" else ">")]
  where
    showAttr :: (Text, Text) -> Text
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

splitAccum :: [Token] -> (Text, [Token])
splitAccum [] = (mempty, [])
splitAccum (reverse -> (Incomplete s : xs)) = (s, reverse xs)
splitAccum tokens = (mempty, tokens)

-- Entities

-- | A conduit to decode entities from a stream of tokens into a new stream of tokens.
decodeEntities :: (Monad m
                  ,Monoid builder)
               => Dec builder Text
               -> Conduit Token m Token
decodeEntities dec =
    start
  where
    start = await >>= maybe (return ()) (\token -> start' token >> start)
    start' (Text t) = (yield t >> yieldWhileText) =$= decodeEntities' dec =$= CL.mapMaybe go
    start' (TagOpen name attrs bool) = yield (TagOpen name (Map.map (decodeString dec) attrs) bool)
    start' token = yield token

    go t
        | t == ""   = Nothing
        | otherwise = Just (Text t)

-- | Decode entities in a complete string.
decodeString
  :: (Eq a, IsString a, Monoid builder, Monoid a)
  => Dec builder a -> a -> a
decodeString dec input =
  case makeEntityDecoder dec input of
    (value', remainder)
      | value' /= mempty -> value' <> decodeString dec remainder
      | otherwise -> input

decodeEntities' :: (Monad m
                   ,Monoid string
                   ,IsString string
                   ,Monoid builder
                   ,Eq string)
                => Dec builder string
                -> Conduit string m string
decodeEntities' dec =
    loop id
  where
    loop accum = do
        mchunk <- await
        let chunk = accum $ fromMaybe mempty mchunk
            (newStr, remainder) = makeEntityDecoder dec chunk
        yield newStr
        if isJust mchunk
            then loop (mappend remainder)
            else yield remainder

-- | Yield contiguous text tokens as strings.
yieldWhileText :: Monad m => Conduit Token m Text
yieldWhileText =
    loop
  where
    loop = await >>= maybe (return ()) go
    go (Text t) = yield t >> loop
    go token = leftover token

-- | A decoder.
data Dec builder string = Dec
  { decToS     :: builder -> string
  , decBreak   :: (Char -> Bool) -> string -> (string,string)
  , decBuilder :: string -> builder
  , decDrop    :: Int -> string -> string
  , decEntity  :: string -> Maybe string
  , decUncons  :: string -> Maybe (Char,string)
  }

-- | Decode the entities in a string type with a decoder.
makeEntityDecoder :: (IsString string,Monoid builder,Eq string,Monoid string)
                  => Dec builder string -> string -> (string, string)
makeEntityDecoder Dec{..} = first decToS . go
  where
    go s =
      case decBreak (=='&') s of
        (_,"") -> (decBuilder s, "")
        (before,restPlusAmp@(decDrop 1 -> rest)) ->
          case decBreak (not . (\c -> isNameChar c || c == '#')) rest of
            (_,"") -> (decBuilder before, restPlusAmp)
            (entity,after) -> (before1 <> before2, after')
              where
                before1 = decBuilder before
                (before2, after') =
                  case mdecoded of
                    Nothing -> first ((decBuilder "&" <> decBuilder entity) <>) (go after)
                    Just (decBuilder -> decoded) ->
                      case decUncons after of
                        Just (';',validAfter) -> first (decoded <>) (go validAfter)
                        Just (_invalid,_rest) -> first (decoded <>) (go after)
                        Nothing -> (mempty, s)
                mdecoded =
                  if entity == mempty
                     then Nothing
                     else decEntity entity

-- | Is the character a valid Name starter?
isNameStart :: Char -> Bool
isNameStart c =
  c == ':' ||
  c == '_' ||
  isAsciiUpper c ||
  isAsciiLower c ||
  (c >= '\xC0' && c <= '\xD6') ||
  (c >= '\xD8' && c <= '\xF6') ||
  (c >= '\xF8' && c <= '\x2FF') ||
  (c >= '\x370' && c <= '\x37D') ||
  (c >= '\x37F' && c <= '\x1FFF') ||
  (c >= '\x200C' && c <= '\x200D') ||
  (c >= '\x2070' && c <= '\x218F') ||
  (c >= '\x2C00' && c <= '\x2FEF') ||
  (c >= '\x3001' && c <= '\xD7FF') ||
  (c >= '\xF900' && c <= '\xFDCF') ||
  (c >= '\xFDF0' && c <= '\xFFFD') ||
  (c >= '\x10000' && c <= '\xEFFFF')

-- | Is the character valid in a Name?
isNameChar :: Char -> Bool
isNameChar c =
  c == '-' ||
  c == '.' ||
  c == '\xB7' ||
  isDigit c ||
  isNameStart c ||
  (c >= '\x0300' && c <= '\x036F') ||
  (c >= '\x203F' && c <= '\x2040')
