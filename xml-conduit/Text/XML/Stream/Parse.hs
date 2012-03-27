{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
-- | This module provides both a native Haskell solution for parsing XML
-- documents into a stream of events, and a set of parser combinators for
-- dealing with a stream of events.
--
-- As a simple example, if you have the following XML file:
--
-- > <?xml version="1.0" encoding="utf-8"?>
-- > <people>
-- >     <person age="25">Michael</person>
-- >     <person age="2">Eliezer</person>
-- > </people>
--
-- Then this code:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Control.Monad.Trans.Resource
-- > import Data.Conduit (($$))
-- > import Data.Text (Text, unpack)
-- > import Text.XML.Stream.Parse
-- >
-- > data Person = Person Int Text
-- >     deriving Show
-- >
-- > parsePerson = tagName "person" (requireAttr "age") $ \age -> do
-- >     name <- content
-- >     return $ Person (read $ unpack age) name
-- >
-- > parsePeople = tagNoAttr "people" $ many parsePerson
-- >
-- > main = do
-- >     people <- runResourceT $
-- >             parseFile def "people.xml" $$ force "people required" parsePeople
-- >     print people
--
-- will produce:
--
-- > [Person {age = 25, name = "Michael"},Person {age = 2, name = "Eliezer"}]
--
-- Previous versions of this module contained a number of more sophisticated
-- functions written by Aristid Breitkreuz and Dmitry Olshansky. To keep this
-- package simpler, those functions are being moved to a separate package. This
-- note will be updated with the name of the package(s) when available.
module Text.XML.Stream.Parse
    ( -- * Parsing XML files
      parseBytes
    , parseText
    , detectUtf
    , parseFile
    , parseLBS
      -- ** Parser settings
    , ParseSettings
    , def
    , DecodeEntities
    , psDecodeEntities
      -- *** Entity decoding
    , decodeXmlEntities
    , decodeHtmlEntities
      -- * Event parsing
    , tag
    , tagPredicate
    , tagName
    , tagNoAttr
    , content
    , contentMaybe
      -- * Attribute parsing
    , AttrParser
    , requireAttr
    , optionalAttr
    , requireAttrRaw
    , optionalAttrRaw
    , ignoreAttrs
      -- * Combinators
    , orE
    , choose
    , many
    , force
      -- * Exceptions
    , XmlException (..)
    ) where
import Data.Attoparsec.Text
    ( char, Parser, takeWhile1, skipWhile, string
    , manyTill, takeWhile, try, anyChar, endOfInput
    )
import qualified Control.Applicative as A
import Data.Conduit.Attoparsec (sinkParser)
import Data.XML.Types
    ( Name (..), Event (..), Content (..)
    , Instruction (..), ExternalID (..)
    )

import Filesystem.Path.CurrentOS (FilePath, encodeString)
import Control.Applicative (Applicative(..), Alternative(empty,(<|>)), (<$>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (Reader, decimal, hexadecimal)
import Data.Text.Encoding (decodeUtf32BEWith)
import Data.Text.Encoding.Error (ignore)
import Data.Word (Word32)
import Blaze.ByteString.Builder (fromWord32be, toByteString)
import Text.XML.Stream.Token
import Prelude hiding (takeWhile, FilePath)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import Control.Monad (ap, liftM)
import qualified Data.Text as TS
import Data.List (foldl')
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.Conduit.Binary (sourceFile)
import Data.Char (isSpace)
import Data.Default (Default (..))
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Resource (MonadResource, monadThrow)
import Control.Monad.Trans.Class (lift)

type Ents = [(Text, Text)]

tokenToEvent :: Ents -> [NSLevel] -> Token -> (Ents, [NSLevel], [Event])
tokenToEvent es n (TokenBeginDocument _) = (es, n, [])
tokenToEvent es n (TokenInstruction i) = (es, n, [EventInstruction i])
tokenToEvent es n (TokenBeginElement name as isClosed _) =
    (es, n', if isClosed then [begin, end] else [begin])
  where
    l0 = case n of
            [] -> NSLevel Nothing Map.empty
            x:_ -> x
    (as', l') = foldl' go (id, l0) as
    go (front, l) (TName kpref kname, val)
        | kpref == Just "xmlns" =
            (front, l { prefixes = Map.insert kname (contentsToText val)
                                 $ prefixes l })
        | kpref == Nothing && kname == "xmlns" =
            (front, l { defaultNS = if T.null $ contentsToText val
                                        then Nothing
                                        else Just $ contentsToText val })
        | otherwise = (front . (:) (TName kpref kname, map resolve val), l)
    resolve (ContentEntity e)
        | Just t <- lookup e es = ContentText t
    resolve c = c
    n' = if isClosed then n else l' : n
    fixAttName level (name', val) = (tnameToName True level name', val)
    begin = EventBeginElement (tnameToName False l' name)
          $ map (fixAttName l')
          $ as' []
    end = EventEndElement $ tnameToName False l' name
tokenToEvent es n (TokenEndElement name) =
    (es, n', [EventEndElement $ tnameToName False l name])
  where
    (l, n') =
        case n of
            [] -> (NSLevel Nothing Map.empty, [])
            x:xs -> (x, xs)
tokenToEvent es n (TokenContent (ContentEntity e))
    | Just t <- lookup e es = (es, n, [EventContent $ ContentText t])
tokenToEvent es n (TokenContent c) = (es, n, [EventContent c])
tokenToEvent es n (TokenComment c) = (es, n, [EventComment c])
tokenToEvent es n (TokenDoctype t eid es') = (es ++ es', n, [EventBeginDoctype t eid, EventEndDoctype])
tokenToEvent es n (TokenCDATA t) = (es, n, [EventCDATA t])

tnameToName :: Bool -> NSLevel -> TName -> Name
tnameToName _ _ (TName (Just "xml") name) =
    Name name (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")
tnameToName isAttr (NSLevel def' _) (TName Nothing name) =
    Name name (if isAttr then Nothing else def') Nothing
tnameToName _ (NSLevel _ m) (TName (Just pref) name) =
    case Map.lookup pref m of
        Just ns -> Name name (Just ns) (Just pref)
        Nothing -> Name name Nothing (Just pref) -- FIXME is this correct?

-- | Automatically determine which UTF variant is being used. This function
-- first checks for BOMs, removing them as necessary, and then check for the
-- equivalent of <?xml for each of UTF-8, UTF-16LE/BE, and UTF-32LE/BE. It
-- defaults to assuming UTF-8.
detectUtf :: C.MonadThrow m => C.Conduit S.ByteString m TS.Text
detectUtf =
    conduit id
  where
    conduit front = C.NeedInput (push front) (return ())

    push front bss =
        case getEncoding front bss of
            Left x -> conduit x
            Right (bss', C.NeedInput decode _) -> decode bss'
            Right _ -> error "detectUtf: Unexpecting decode constructor"

    getEncoding front bs'
        | S.length bs < 4 =
            Left (bs `S.append`)
        | otherwise = do
            let decode = CT.decode codec
             in Right (bsOut, decode)
      where
        bs = front bs'
        bsOut = S.append (S.drop toDrop x) y
        (x, y) = S.splitAt 4 bs
        (toDrop, codec) =
            case S.unpack x of
                [0x00, 0x00, 0xFE, 0xFF] -> (4, CT.utf32_be)
                [0xFF, 0xFE, 0x00, 0x00] -> (4, CT.utf32_le)
                0xFE : 0xFF: _           -> (2, CT.utf16_be)
                0xFF : 0xFE: _           -> (2, CT.utf16_le)
                0xEF : 0xBB: 0xBF : _    -> (3, CT.utf8)
                [0x00, 0x00, 0x00, 0x3C] -> (0, CT.utf32_be)
                [0x3C, 0x00, 0x00, 0x00] -> (0, CT.utf32_le)
                [0x00, 0x3C, 0x00, 0x3F] -> (0, CT.utf16_be)
                [0x3C, 0x00, 0x3F, 0x00] -> (0, CT.utf16_le)
                _                        -> (0, CT.utf8) -- Assuming UTF-8

-- | Parses a byte stream into 'Event's. This function is implemented fully in
-- Haskell using attoparsec-text for parsing. The produced error messages do
-- not give line/column information, so you may prefer to stick with the parser
-- provided by libxml-enumerator. However, this has the advantage of not
-- relying on any C libraries.
--
-- This relies on 'detectUtf' to determine character encoding, and 'parseText'
-- to do the actual parsing.
parseBytes :: C.MonadThrow m
           => ParseSettings -> C.Conduit S.ByteString m Event
parseBytes ps = detectUtf C.=$= parseText ps

dropBOM :: Monad m => C.Conduit TS.Text m TS.Text
dropBOM = C.conduitState
    False
    push
    close
  where
    push a b = do
        let (a', b') = go a b
        return $ C.StateProducing a' [b']
    close _ = return []
    go True b = (True, b)
    go False b =
        case T.uncons b of
            Nothing -> (False, b)
            Just (c, cs) -> (True,
                if c == '\xfeef' then cs else b)

-- | Parses a character stream into 'Event's. This function is implemented
-- fully in Haskell using attoparsec-text for parsing. The produced error
-- messages do not give line/column information, so you may prefer to stick
-- with the parser provided by libxml-enumerator. However, this has the
-- advantage of not relying on any C libraries.
parseText :: C.MonadThrow m
          => ParseSettings
          -> C.Conduit TS.Text m Event
parseText de =
    dropBOM
        C.=$= tokenize
        C.=$= toEventC
        C.=$= addBeginEnd
  where
    tokenize = C.sequenceSink () $ const $ C.Emit () . return <$> sinkToken de
    addBeginEnd = C.conduitState
        False
        push
        close
      where
        go False es = EventBeginDocument : [es]
        go True es = [es]

        push x es = return $ C.StateProducing True $ go x es
        close x = return $ go x EventEndDocument

toEventC :: Monad m => C.Conduit (Maybe Token) m Event
toEventC = C.conduitState
    ([], [])
    push
    close
  where
    go es levels [] front = (es, levels, front [])
    go es levels (t:ts) front =
        let (es', levels', events) = tokenToEvent es levels t
         in go es' levels' ts (front . (events ++))

    push (es, levels) tokens =
        return $ C.StateProducing (es', levels') events
      where
        (es', levels', events) = go es levels (catMaybes [tokens]) id

    close (es, levels) =
        return events
      where
        (_, _, events) = go es levels (catMaybes []) id

data ParseSettings = ParseSettings
    { psDecodeEntities :: DecodeEntities
    }

instance Default ParseSettings where
    def = ParseSettings
        { psDecodeEntities = decodeXmlEntities
        }

sinkToken :: C.MonadThrow m => ParseSettings -> C.Sink TS.Text m (Maybe Token)
sinkToken de = sinkParser ((endOfInput >> return Nothing) <|> fmap Just (parseToken $ psDecodeEntities de))

parseToken :: DecodeEntities -> Parser Token
parseToken de = (char '<' >> parseLt) <|> TokenContent <$> parseContent de False False
  where
    parseLt =
        (char '?' >> parseInstr) <|>
        (char '!' >> (parseComment <|> parseCdata <|> parseDoctype)) <|>
        (char '/' >> parseEnd) <|>
        parseBegin
    parseInstr = do
        name <- parseIdent
        if name == "xml"
            then do
                as <- A.many $ parseAttribute de
                skipSpace
                char' '?'
                char' '>'
                newline <|> return ()
                return $ TokenBeginDocument as
            else do
                skipSpace
                x <- T.pack <$> manyTill anyChar (try $ string "?>")
                return $ TokenInstruction $ Instruction name x
    parseComment = do
        char' '-'
        char' '-'
        c <- T.pack <$> manyTill anyChar (string "-->") -- FIXME use takeWhile instead
        return $ TokenComment c
    parseCdata = do
        _ <- string "[CDATA["
        t <- T.pack <$> manyTill anyChar (string "]]>") -- FIXME use takeWhile instead
        return $ TokenCDATA t
    parseDoctype = do
        _ <- string "DOCTYPE"
        skipSpace
        i <- parseIdent
        skipSpace
        eid <- fmap Just parsePublicID <|>
               fmap Just parseSystemID <|>
               return Nothing
        skipSpace
        ents <- (do
            char' '['
            ents <- parseEntities id
            skipSpace
            return ents) <|> return []
        char' '>'
        newline <|> return ()
        return $ TokenDoctype i eid ents
    parseEntities front =
        (char ']' >> return (front [])) <|>
        (parseEntity >>= \e -> parseEntities (front . (e:))) <|>
        (char '<' >> parseEntities front) <|>
        (skipWhile (\t -> t /= ']' && t /= '<') >> parseEntities front)
    parseEntity = try $ do
        _ <- string "<!ENTITY"
        skipSpace
        i <- parseIdent
        t <- quotedText
        skipSpace
        char' '>'
        return (i, t)
    parsePublicID = do
        _ <- string "PUBLIC"
        x <- quotedText
        y <- quotedText
        return $ PublicID x y
    parseSystemID = do
        _ <- string "SYSTEM"
        x <- quotedText
        return $ SystemID x
    quotedText = do
        skipSpace
        between '"' <|> between '\''
    between c = do
        char' c
        x <- takeWhile (/=c)
        char' c
        return x
    parseEnd = do
        skipSpace
        n <- parseName
        skipSpace
        char' '>'
        return $ TokenEndElement n
    parseBegin = do
        skipSpace
        n <- parseName
        as <- A.many $ parseAttribute de
        skipSpace
        isClose <- (char '/' >> skipSpace >> return True) <|> return False
        char' '>'
        return $ TokenBeginElement n as isClose 0

parseAttribute :: DecodeEntities -> Parser TAttribute
parseAttribute de = do
    skipSpace
    key <- parseName
    skipSpace
    char' '='
    skipSpace
    val <- squoted <|> dquoted
    return (key, val)
  where
    squoted = do
        char' '\''
        manyTill (parseContent de False True) (char '\'')
    dquoted = do
        char' '"'
        manyTill (parseContent de True False) (char '"')

parseName :: Parser TName
parseName = do
    i1 <- parseIdent
    mi2 <- (char ':' >> fmap Just parseIdent) <|> return Nothing
    return $
        case mi2 of
            Nothing -> TName Nothing i1
            Just i2 -> TName (Just i1) i2

parseIdent :: Parser Text
parseIdent =
    takeWhile1 valid
  where
    valid '&' = False
    valid '<' = False
    valid '>' = False
    valid ':' = False
    valid '?' = False
    valid '=' = False
    valid '"' = False
    valid '\'' = False
    valid '/' = False
    valid c  = not $ isSpace c

parseContent :: DecodeEntities
             -> Bool -- break on double quote
             -> Bool -- break on single quote
             -> Parser Content
parseContent de breakDouble breakSingle =
    parseEntity <|> parseText'
  where
    parseEntity = do
        char' '&'
        t <- takeWhile1 (/= ';')
        char' ';'
        return $ de t
    parseText' = do
        bs <- takeWhile1 valid
        return $ ContentText bs
    valid '"' = not breakDouble
    valid '\'' = not breakSingle
    valid '&' = False -- amp
    valid '<' = False -- lt
    valid _  = True

skipSpace :: Parser ()
skipSpace = skipWhile isSpace

newline :: Parser ()
newline = ((char '\r' >> char '\n') <|> char '\n') >> return ()

char' :: Char -> Parser ()
char' c = char c >> return ()

data ContentType =
    Ignore | IsContent Text | IsError String | NotContent

-- | Grabs the next piece of content if available. This function skips over any
-- comments and instructions and concatenates all content until the next start
-- or end tag.
contentMaybe :: C.MonadThrow m => C.Sink Event m (Maybe Text)
contentMaybe = do
    x <- CL.peek
    case pc' x of
        Ignore -> CL.drop 1 >> contentMaybe
        IsContent t -> CL.drop 1 >> fmap Just (takeContents (t:))
        IsError e -> lift $ C.monadThrow $ XmlException e x
        NotContent -> return Nothing
  where
    pc' Nothing = NotContent
    pc' (Just x) = pc x
    pc (EventContent (ContentText t)) = IsContent t
    pc (EventContent (ContentEntity e)) = IsError $ "Unknown entity: " ++ show e
    pc (EventCDATA t) = IsContent t
    pc EventBeginElement{} = NotContent
    pc EventEndElement{} = NotContent
    pc EventBeginDocument{} = Ignore
    pc EventEndDocument = Ignore
    pc EventBeginDoctype{} = Ignore
    pc EventEndDoctype = Ignore
    pc EventInstruction{} = Ignore
    pc EventComment{} = Ignore
    takeContents front = do
        x <- CL.peek
        case pc' x of
            Ignore -> CL.drop 1 >> takeContents front
            IsContent t -> CL.drop 1 >> takeContents (front . (:) t)
            IsError e -> lift $ C.monadThrow $ XmlException e x
            NotContent -> return $ T.concat $ front []

-- | Grabs the next piece of content. If none if available, returns 'T.empty'.
-- This is simply a wrapper around 'contentMaybe'.
content :: C.MonadThrow m => C.Sink Event m Text
content = do
    x <- contentMaybe
    case x of
        Nothing -> return T.empty
        Just y -> return y

-- | The most generic way to parse a tag. It takes a predicate for checking if
-- this is the correct tag name, an 'AttrParser' for handling attributes, and
-- then a parser for dealing with content.
--
-- This function automatically absorbs its balancing closing tag, and will
-- throw an exception if not all of the attributes or child elements are
-- consumed. If you want to allow extra attributes, see 'ignoreAttrs'.
--
-- This function automatically ignores comments, instructions and whitespace.
tag :: C.MonadThrow m
    => (Name -> Maybe a)
    -> (a -> AttrParser b)
    -> (b -> C.Sink Event m c)
    -> C.Sink Event m (Maybe c)
tag checkName attrParser f = do
    x <- dropWS
    case x of
        Just (EventBeginElement name as) ->
            case checkName name of
                Just y ->
                    case runAttrParser' (attrParser y) as of
                        Left e -> lift $ C.monadThrow e
                        Right z -> do
                            CL.drop 1
                            z' <- f z
                            a <- dropWS
                            case a of
                                Just (EventEndElement name')
                                    | name == name' -> CL.drop 1 >> return (Just z')
                                _ -> lift $ C.monadThrow $ XmlException ("Expected end tag for: " ++ show name) a
                Nothing -> return Nothing
        _ -> return Nothing
  where
    dropWS = do
        x <- CL.peek
        let isWS =
                case x of
                    Just EventBeginDocument -> True
                    Just EventEndDocument -> True
                    Just EventBeginDoctype{} -> True
                    Just EventEndDoctype -> True
                    Just EventInstruction{} -> True
                    Just EventBeginElement{} -> False
                    Just EventEndElement{} -> False
                    Just (EventContent (ContentText t))
                        | T.all isSpace t -> True
                        | otherwise -> False
                    Just (EventContent ContentEntity{}) -> False
                    Just EventComment{} -> True
                    Just EventCDATA{} -> False
                    Nothing -> False
        if isWS then CL.drop 1 >> dropWS else return x
    runAttrParser' p as =
        case runAttrParser p as of
            Left e -> Left e
            Right ([], x) -> Right x
            Right (attr, _) -> Left $ UnparsedAttributes attr

-- | A simplified version of 'tag' which matches against boolean predicates.
tagPredicate :: C.MonadThrow m
             => (Name -> Bool)
             -> AttrParser a
             -> (a -> C.Sink Event m b)
             -> C.Sink Event m (Maybe b)
tagPredicate p attrParser = tag (\x -> if p x then Just () else Nothing) (const attrParser)

-- | A simplified version of 'tag' which matches for specific tag names instead
-- of taking a predicate function. This is often sufficient, and when combined
-- with OverloadedStrings and the IsString instance of 'Name', can prove to be
-- very concise.
tagName :: C.MonadThrow m
     => Name
     -> AttrParser a
     -> (a -> C.Sink Event m b)
     -> C.Sink Event m (Maybe b)
tagName name = tagPredicate (== name)

-- | A further simplified tag parser, which requires that no attributes exist.
tagNoAttr :: C.MonadThrow m => Name -> C.Sink Event m a -> C.Sink Event m (Maybe a)
tagNoAttr name f = tagName name (return ()) $ const f

-- | Get the value of the first parser which returns 'Just'. If no parsers
-- succeed (i.e., return 'Just'), this function returns 'Nothing'.
--
-- > orE a b = choose [a, b]
orE :: Monad m => C.Sink Event m (Maybe a) -> C.Sink Event m (Maybe a) -> C.Sink Event m (Maybe a)
orE a b = do
  x <- a
  case x of
    Nothing -> b
    _ -> return x

-- | Get the value of the first parser which returns 'Just'. If no parsers
-- succeed (i.e., return 'Just'), this function returns 'Nothing'.
choose :: Monad m
       => [C.Sink Event m (Maybe a)]
       -> C.Sink Event m (Maybe a)
choose [] = return Nothing
choose (i:is) = do
    x <- i
    case x of
        Nothing -> choose is
        Just a -> return $ Just a

-- | Force an optional parser into a required parser. All of the 'tag'
-- functions, 'choose' and 'many' deal with 'Maybe' parsers. Use this when you
-- want to finally force something to happen.
force :: C.MonadThrow m
      => String -- ^ Error message
      -> C.Sink Event m (Maybe a)
      -> C.Sink Event m a
force msg i = do
    x <- i
    case x of
        Nothing -> lift $ monadThrow $ XmlException msg Nothing
        Just a -> return a

-- | A helper function which reads a file from disk using 'enumFile', detects
-- character encoding using 'detectUtf', parses the XML using 'parseBytes', and
-- then hands off control to your supplied parser.
parseFile :: MonadResource m
          => ParseSettings
          -> FilePath
          -> C.Source m Event
parseFile ps fp = sourceFile (encodeString fp) C.$= parseBytes ps

-- | Parse an event stream from a lazy 'L.ByteString'.
parseLBS :: C.MonadThrow m
         => ParseSettings
         -> L.ByteString
         -> C.Source m Event
parseLBS ps lbs = CL.sourceList (L.toChunks lbs) C.$= parseBytes ps

data XmlException = XmlException
    { xmlErrorMessage :: String
    , xmlBadInput :: Maybe Event
    }
                  | InvalidEndElement Name
                  | InvalidEntity Text
                  | UnparsedAttributes [(Name, [Content])]
    deriving (Show, Typeable)
instance Exception XmlException

-- | A monad for parsing attributes. By default, it requires you to deal with
-- all attributes present on an element, and will throw an exception if there
-- are unhandled attributes. Use the 'requireAttr', 'optionalAttr' et al
-- functions for handling an attribute, and 'ignoreAttrs' if you would like to
-- skip the rest of the attributes on an element.
--
-- 'Alternative' instance behave like 'First' monoid. It chooses first
-- parser which doesn't fail.
newtype AttrParser a = AttrParser { runAttrParser :: [(Name, [Content])] -> Either XmlException ([(Name, [Content])], a) }

instance Monad AttrParser where
    return a = AttrParser $ \as -> Right (as, a)
    (AttrParser f) >>= g = AttrParser $ \as ->
        case f as of
            Left e -> Left e
            Right (as', f') -> runAttrParser (g f') as'
instance Functor AttrParser where
    fmap = liftM
instance Applicative AttrParser where
    pure = return
    (<*>) = ap
instance Alternative AttrParser where
    empty = AttrParser $ const $ Left $ XmlException "AttrParser.empty" Nothing
    AttrParser f <|> AttrParser g = AttrParser $ \x ->
      case f x of
        Left  _ -> g x
        res     -> res

optionalAttrRaw :: ((Name, [Content]) -> Maybe b) -> AttrParser (Maybe b)
optionalAttrRaw f =
    AttrParser $ go id
  where
    go front [] = Right (front [], Nothing)
    go front (a:as) =
        case f a of
            Nothing -> go (front . (:) a) as
            Just b -> Right (front as, Just b)

requireAttrRaw :: String -> ((Name, [Content]) -> Maybe b) -> AttrParser b
requireAttrRaw msg f = do
    x <- optionalAttrRaw f
    case x of
        Just b -> return b
        Nothing -> AttrParser $ const $ Left $ XmlException msg Nothing

-- | Require that a certain attribute be present and return its value.
requireAttr :: Name -> AttrParser Text
requireAttr n = requireAttrRaw
    ("Missing attribute: " ++ show n)
    (\(x, y) -> if x == n then Just (contentsToText y) else Nothing)

-- | Return the value for an attribute if present.
optionalAttr :: Name -> AttrParser (Maybe Text)
optionalAttr n = optionalAttrRaw
    (\(x, y) -> if x == n then Just (contentsToText y) else Nothing)

contentsToText :: [Content] -> Text
contentsToText =
    T.concat . map toText
  where
    toText (ContentText t) = t
    toText (ContentEntity e) = T.concat ["&", e, ";"]

-- | Skip the remaining attributes on an element. Since this will clear the
-- list of attributes, you must call this /after/ any calls to 'requireAttr',
-- 'optionalAttr', etc.
ignoreAttrs :: AttrParser ()
ignoreAttrs = AttrParser $ \_ -> Right ([], ())

-- | Keep parsing elements as long as the parser returns 'Just'.
many :: Monad m => C.Sink Event m (Maybe a) -> C.Sink Event m [a]
many i =
    go id
  where
    go front = do
        x <- i
        case x of
            Nothing -> return $ front []
            Just y -> go $ front . (:) y

type DecodeEntities = Text -> Content

-- | Default implementation of 'DecodeEntities': handles numeric entities and
-- the five standard character entities (lt, gt, amp, quot, apos).
decodeXmlEntities :: DecodeEntities
decodeXmlEntities "lt" = ContentText "<"
decodeXmlEntities "gt" = ContentText ">"
decodeXmlEntities "amp" = ContentText "&"
decodeXmlEntities "quot" = ContentText "\""
decodeXmlEntities "apos" = ContentText "'"
decodeXmlEntities t = let backup = ContentEntity t in
  case T.uncons t of
    Just ('#', t') ->
      case T.uncons t' of
        Just ('x', t'')
          | T.length t'' > 6 -> backup
          | otherwise        -> decodeChar hexadecimal backup t''
        _
          | T.length t'  > 7 -> backup
          | otherwise        -> decodeChar decimal backup t'
    _ -> backup

-- | HTML4-compliant entity decoder. Handles numerics, the five standard
-- character entities, and the additional 248 entities defined by HTML 4 and
-- XHTML 1.
--
-- Note that HTML 5 introduces a drastically larger number of entities, and
-- this code does not recognize most of them.
decodeHtmlEntities :: DecodeEntities
decodeHtmlEntities t =
    case decodeXmlEntities t of
        x@ContentText{} -> x
        backup@ContentEntity{} ->
            case Map.lookup t htmlEntities of
                Just x -> ContentText x
                Nothing -> backup

htmlEntities :: Map.Map T.Text T.Text
htmlEntities = Map.fromList
    [ ("nbsp", "\160")
    , ("iexcl", "\161")
    , ("cent", "\162")
    , ("pound", "\163")
    , ("curren", "\164")
    , ("yen", "\165")
    , ("brvbar", "\166")
    , ("sect", "\167")
    , ("uml", "\168")
    , ("copy", "\169")
    , ("ordf", "\170")
    , ("laquo", "\171")
    , ("not", "\172")
    , ("shy", "\173")
    , ("reg", "\174")
    , ("macr", "\175")
    , ("deg", "\176")
    , ("plusmn", "\177")
    , ("sup2", "\178")
    , ("sup3", "\179")
    , ("acute", "\180")
    , ("micro", "\181")
    , ("para", "\182")
    , ("middot", "\183")
    , ("cedil", "\184")
    , ("sup1", "\185")
    , ("ordm", "\186")
    , ("raquo", "\187")
    , ("frac14", "\188")
    , ("frac12", "\189")
    , ("frac34", "\190")
    , ("iquest", "\191")
    , ("Agrave", "\192")
    , ("Aacute", "\193")
    , ("Acirc", "\194")
    , ("Atilde", "\195")
    , ("Auml", "\196")
    , ("Aring", "\197")
    , ("AElig", "\198")
    , ("Ccedil", "\199")
    , ("Egrave", "\200")
    , ("Eacute", "\201")
    , ("Ecirc", "\202")
    , ("Euml", "\203")
    , ("Igrave", "\204")
    , ("Iacute", "\205")
    , ("Icirc", "\206")
    , ("Iuml", "\207")
    , ("ETH", "\208")
    , ("Ntilde", "\209")
    , ("Ograve", "\210")
    , ("Oacute", "\211")
    , ("Ocirc", "\212")
    , ("Otilde", "\213")
    , ("Ouml", "\214")
    , ("times", "\215")
    , ("Oslash", "\216")
    , ("Ugrave", "\217")
    , ("Uacute", "\218")
    , ("Ucirc", "\219")
    , ("Uuml", "\220")
    , ("Yacute", "\221")
    , ("THORN", "\222")
    , ("szlig", "\223")
    , ("agrave", "\224")
    , ("aacute", "\225")
    , ("acirc", "\226")
    , ("atilde", "\227")
    , ("auml", "\228")
    , ("aring", "\229")
    , ("aelig", "\230")
    , ("ccedil", "\231")
    , ("egrave", "\232")
    , ("eacute", "\233")
    , ("ecirc", "\234")
    , ("euml", "\235")
    , ("igrave", "\236")
    , ("iacute", "\237")
    , ("icirc", "\238")
    , ("iuml", "\239")
    , ("eth", "\240")
    , ("ntilde", "\241")
    , ("ograve", "\242")
    , ("oacute", "\243")
    , ("ocirc", "\244")
    , ("otilde", "\245")
    , ("ouml", "\246")
    , ("divide", "\247")
    , ("oslash", "\248")
    , ("ugrave", "\249")
    , ("uacute", "\250")
    , ("ucirc", "\251")
    , ("uuml", "\252")
    , ("yacute", "\253")
    , ("thorn", "\254")
    , ("yuml", "\255")
    , ("OElig", "\338")
    , ("oelig", "\339")
    , ("Scaron", "\352")
    , ("scaron", "\353")
    , ("Yuml", "\376")
    , ("fnof", "\402")
    , ("circ", "\710")
    , ("tilde", "\732")
    , ("Alpha", "\913")
    , ("Beta", "\914")
    , ("Gamma", "\915")
    , ("Delta", "\916")
    , ("Epsilon", "\917")
    , ("Zeta", "\918")
    , ("Eta", "\919")
    , ("Theta", "\920")
    , ("Iota", "\921")
    , ("Kappa", "\922")
    , ("Lambda", "\923")
    , ("Mu", "\924")
    , ("Nu", "\925")
    , ("Xi", "\926")
    , ("Omicron", "\927")
    , ("Pi", "\928")
    , ("Rho", "\929")
    , ("Sigma", "\931")
    , ("Tau", "\932")
    , ("Upsilon", "\933")
    , ("Phi", "\934")
    , ("Chi", "\935")
    , ("Psi", "\936")
    , ("Omega", "\937")
    , ("alpha", "\945")
    , ("beta", "\946")
    , ("gamma", "\947")
    , ("delta", "\948")
    , ("epsilon", "\949")
    , ("zeta", "\950")
    , ("eta", "\951")
    , ("theta", "\952")
    , ("iota", "\953")
    , ("kappa", "\954")
    , ("lambda", "\955")
    , ("mu", "\956")
    , ("nu", "\957")
    , ("xi", "\958")
    , ("omicron", "\959")
    , ("pi", "\960")
    , ("rho", "\961")
    , ("sigmaf", "\962")
    , ("sigma", "\963")
    , ("tau", "\964")
    , ("upsilon", "\965")
    , ("phi", "\966")
    , ("chi", "\967")
    , ("psi", "\968")
    , ("omega", "\969")
    , ("thetasym", "\977")
    , ("upsih", "\978")
    , ("piv", "\982")
    , ("ensp", "\8194")
    , ("emsp", "\8195")
    , ("thinsp", "\8201")
    , ("zwnj", "\8204")
    , ("zwj", "\8205")
    , ("lrm", "\8206")
    , ("rlm", "\8207")
    , ("ndash", "\8211")
    , ("mdash", "\8212")
    , ("lsquo", "\8216")
    , ("rsquo", "\8217")
    , ("sbquo", "\8218")
    , ("ldquo", "\8220")
    , ("rdquo", "\8221")
    , ("bdquo", "\8222")
    , ("dagger", "\8224")
    , ("Dagger", "\8225")
    , ("bull", "\8226")
    , ("hellip", "\8230")
    , ("permil", "\8240")
    , ("prime", "\8242")
    , ("Prime", "\8243")
    , ("lsaquo", "\8249")
    , ("rsaquo", "\8250")
    , ("oline", "\8254")
    , ("frasl", "\8260")
    , ("euro", "\8364")
    , ("image", "\8465")
    , ("weierp", "\8472")
    , ("real", "\8476")
    , ("trade", "\8482")
    , ("alefsym", "\8501")
    , ("larr", "\8592")
    , ("uarr", "\8593")
    , ("rarr", "\8594")
    , ("darr", "\8595")
    , ("harr", "\8596")
    , ("crarr", "\8629")
    , ("lArr", "\8656")
    , ("uArr", "\8657")
    , ("rArr", "\8658")
    , ("dArr", "\8659")
    , ("hArr", "\8660")
    , ("forall", "\8704")
    , ("part", "\8706")
    , ("exist", "\8707")
    , ("empty", "\8709")
    , ("nabla", "\8711")
    , ("isin", "\8712")
    , ("notin", "\8713")
    , ("ni", "\8715")
    , ("prod", "\8719")
    , ("sum", "\8721")
    , ("minus", "\8722")
    , ("lowast", "\8727")
    , ("radic", "\8730")
    , ("prop", "\8733")
    , ("infin", "\8734")
    , ("ang", "\8736")
    , ("and", "\8743")
    , ("or", "\8744")
    , ("cap", "\8745")
    , ("cup", "\8746")
    , ("int", "\8747")
    , ("there4", "\8756")
    , ("sim", "\8764")
    , ("cong", "\8773")
    , ("asymp", "\8776")
    , ("ne", "\8800")
    , ("equiv", "\8801")
    , ("le", "\8804")
    , ("ge", "\8805")
    , ("sub", "\8834")
    , ("sup", "\8835")
    , ("nsub", "\8836")
    , ("sube", "\8838")
    , ("supe", "\8839")
    , ("oplus", "\8853")
    , ("otimes", "\8855")
    , ("perp", "\8869")
    , ("sdot", "\8901")
    , ("lceil", "\8968")
    , ("rceil", "\8969")
    , ("lfloor", "\8970")
    , ("rfloor", "\8971")
    , ("lang", "\9001")
    , ("rang", "\9002")
    , ("loz", "\9674")
    , ("spades", "\9824")
    , ("clubs", "\9827")
    , ("hearts", "\9829")
    , ("diams", "\9830")
    ]

decodeChar :: Reader Word32 -> Content -> Text -> Content
decodeChar readNum backup = either (const backup) toContent . readNum
  where
    toContent (num, extra) | T.null extra =
      case decodeUtf32BEWith ignore . toByteString $ fromWord32be num of
          c    | T.length c    == 1 -> ContentText c
               | otherwise          -> backup
    toContent _ = backup
