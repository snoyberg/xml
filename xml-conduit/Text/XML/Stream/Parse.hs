{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}
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
    , parseBytesPos
    , parseText'
    , parseText
    , parseTextPos
    , detectUtf
    , parseFile
    , parseLBS
      -- ** Parser settings
    , ParseSettings
    , def
    , DecodeEntities
    , psDecodeEntities
    , psRetainNamespaces
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
      -- * Other types
    , PositionRange
    , EventPos
    ) where
import Data.Attoparsec.Text
    ( char, Parser, takeWhile1, skipWhile, string
    , manyTill, takeWhile, try, anyChar
    )
import qualified Control.Applicative as A
import Control.Monad.Trans.Resource (MonadThrow, monadThrow, MonadResource)
import Data.Conduit.Attoparsec (conduitParser, PositionRange)
import Data.XML.Types
    ( Name (..), Event (..), Content (..)
    , Instruction (..), ExternalID (..)
    )

import Filesystem.Path.CurrentOS (FilePath, encodeString)
import Control.Applicative (Applicative(..), Alternative(empty,(<|>)), (<$>))
import Data.Text (Text, pack)
import Control.Arrow ((***))
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
import Data.Conduit
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Internal as CI
import Control.Monad (ap, liftM, void, guard)
import qualified Data.Text as TS
import Data.List (foldl')
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.Conduit.Binary (sourceFile)
import Data.Char (isSpace)
import Data.Default (Default (..))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe, isNothing)

type Ents = [(Text, Text)]

tokenToEvent :: ParseSettings -> Ents -> [NSLevel] -> Token -> (Ents, [NSLevel], [Event])
tokenToEvent _ es n (TokenBeginDocument _) = (es, n, [])
tokenToEvent _ es n (TokenInstruction i) = (es, n, [EventInstruction i])
tokenToEvent ps es n (TokenBeginElement name as isClosed _) =
    (es, n', if isClosed then [begin, end] else [begin])
  where
    l0 = case n of
            [] -> NSLevel Nothing Map.empty
            x:_ -> x
    (as', l') = foldl' go (id, l0) as
    go (front, l) (TName kpref kname, val) =
        (addNS front, l'')
      where
        isPrefixed = kpref == Just "xmlns"
        isUnprefixed = isNothing kpref && kname == "xmlns"

        addNS
            | not (psRetainNamespaces ps) && (isPrefixed || isUnprefixed) = id
            | otherwise = (((tname, map resolve val):) .)
          where
            tname
                | isPrefixed = TName Nothing ("xmlns:" `T.append` kname)
                | otherwise = TName kpref kname

        l''
            | isPrefixed =
                l { prefixes = Map.insert kname (contentsToText val)
                                     $ prefixes l }
            | isUnprefixed =
                l { defaultNS = if T.null $ contentsToText val
                                            then Nothing
                                            else Just $ contentsToText val }
            | otherwise = l

    resolve (ContentEntity e)
        | Just t <- lookup e es = ContentText t
    resolve c = c
    n' = if isClosed then n else l' : n
    fixAttName (name', val) = (tnameToName True l' name', val)
    elementName = tnameToName False l' name
    begin = EventBeginElement elementName $ map fixAttName $ as' []
    end = EventEndElement elementName
tokenToEvent _ es n (TokenEndElement name) =
    (es, n', [EventEndElement $ tnameToName False l name])
  where
    (l, n') =
        case n of
            [] -> (NSLevel Nothing Map.empty, [])
            x:xs -> (x, xs)
tokenToEvent _ es n (TokenContent (ContentEntity e))
    | Just t <- lookup e es = (es, n, [EventContent $ ContentText t])
tokenToEvent _ es n (TokenContent c) = (es, n, [EventContent c])
tokenToEvent _ es n (TokenComment c) = (es, n, [EventComment c])
tokenToEvent _ es n (TokenDoctype t eid es') = (es ++ es', n, [EventBeginDoctype t eid, EventEndDoctype])
tokenToEvent _ es n (TokenCDATA t) = (es, n, [EventCDATA t])

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
detectUtf :: MonadThrow m => Conduit S.ByteString m TS.Text
detectUtf =
    conduit id
  where
    conduit front = await >>= maybe (return ()) (push front)

    push front bss =
        either conduit (\(bss', continue) -> leftover bss' >> continue)
               (getEncoding front bss)

    getEncoding front bs'
        | S.length bs < 4 =
            Left (bs `S.append`)
        | otherwise =
            Right (bsOut, CT.decode codec)
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

type EventPos = (Maybe PositionRange, Event)

-- | Parses a byte stream into 'Event's. This function is implemented fully in
-- Haskell using attoparsec-text for parsing. The produced error messages do
-- not give line/column information, so you may prefer to stick with the parser
-- provided by libxml-enumerator. However, this has the advantage of not
-- relying on any C libraries.
--
-- This relies on 'detectUtf' to determine character encoding, and 'parseText''
-- to do the actual parsing.
parseBytes :: MonadThrow m
           => ParseSettings
           -> Conduit S.ByteString m Event
parseBytes = mapOutput snd . parseBytesPos

parseBytesPos :: MonadThrow m
              => ParseSettings
              -> Conduit S.ByteString m EventPos
parseBytesPos ps = detectUtf =$= parseTextPos ps

dropBOM :: Monad m => Conduit TS.Text m TS.Text
dropBOM =
    await >>= maybe (return ()) push
  where
    push t =
        case T.uncons t of
            Nothing -> dropBOM
            Just (c, cs) ->
                let output
                        | c == '\xfeef' = cs
                        | otherwise = t
                 in yield output >> idConduit
    idConduit = await >>= maybe (return ()) (\x -> yield x >> idConduit)

-- | Parses a character stream into 'Event's. This function is implemented
-- fully in Haskell using attoparsec-text for parsing. The produced error
-- messages do not give line/column information, so you may prefer to stick
-- with the parser provided by libxml-enumerator. However, this has the
-- advantage of not relying on any C libraries.
parseText' :: MonadThrow m
           => ParseSettings
           -> Conduit TS.Text m Event
parseText' = mapOutput snd . parseTextPos

{-# DEPRECATED parseText "Please use 'parseText'' or 'parseTextPos'." #-}
parseText :: MonadThrow m
          => ParseSettings
          -> Conduit TS.Text m EventPos
parseText = parseTextPos

parseTextPos :: MonadThrow m
          => ParseSettings
          -> Conduit TS.Text m EventPos
parseTextPos de =
    dropBOM
        =$= tokenize
        =$= toEventC de
        =$= addBeginEnd
  where
    tokenize = conduitToken de
    addBeginEnd = yield (Nothing, EventBeginDocument) >> addEnd
    addEnd = await >>= maybe
        (yield (Nothing, EventEndDocument))
        (\e -> yield e >> addEnd)

toEventC :: Monad m => ParseSettings -> Conduit (PositionRange, Token) m EventPos
toEventC ps =
    go [] []
  where
    go !es !levels =
        await >>= maybe (return ()) push
      where
        push (position, token) =
            mapM_ (yield . (,) (Just position)) events >> go es' levels'
          where
            (es', levels', events) = tokenToEvent ps es levels token

data ParseSettings = ParseSettings
    { psDecodeEntities :: DecodeEntities
    , psRetainNamespaces :: Bool
    -- ^ Whether the original xmlns attributes should be retained in the parsed
    -- values. For more information on motivation, see:
    --
    -- <https://github.com/snoyberg/xml/issues/38>
    --
    -- Default: False
    --
    -- Since 1.2.1
    }

instance Default ParseSettings where
    def = ParseSettings
        { psDecodeEntities = decodeXmlEntities
        , psRetainNamespaces = False
        }

conduitToken :: MonadThrow m => ParseSettings -> Conduit TS.Text m (PositionRange, Token)
conduitToken = conduitParser . parseToken . psDecodeEntities

parseToken :: DecodeEntities -> Parser Token
parseToken de = (char '<' >> parseLt) <|> TokenContent <$> parseContent de False False
  where
    parseLt =
        (char '?' >> parseInstr) <|>
        (char '!' >> (parseComment <|> parseCdata <|> parseDoctype)) <|>
        parseBegin <|>
        (char '/' >> parseEnd)
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
        name <- parseName
        let i =
                case name of
                    TName Nothing x -> x
                    TName (Just x) y -> T.concat [x, ":", y]
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
    squoted = char '\'' *> manyTill (parseContent de False True) (char '\'')
    dquoted = char  '"' *> manyTill (parseContent de True False) (char  '"')

parseName :: Parser TName
parseName =
  name <$> parseIdent <*> A.optional (char ':' >> parseIdent)
  where
    name i1 Nothing = TName Nothing i1
    name i1 (Just i2) = TName (Just i1) i2

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
    valid c  = not $ isXMLSpace c

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
skipSpace = skipWhile isXMLSpace

-- | Determines whether a character is an XML white space. The list of
-- white spaces is given by
--
-- >  S ::= (#x20 | #x9 | #xD | #xA)+
--
-- in <http://www.w3.org/TR/2008/REC-xml-20081126/#sec-common-syn>.
isXMLSpace :: Char -> Bool
isXMLSpace ' ' = True
isXMLSpace '\t' = True
isXMLSpace '\r' = True
isXMLSpace '\n' = True
isXMLSpace _ = False

newline :: Parser ()
newline = ((char '\r' >> char '\n') <|> char '\n') >> return ()

char' :: Char -> Parser ()
char' = void . char

data ContentType =
    Ignore | IsContent Text | IsError String | NotContent

-- | Grabs the next piece of content if available. This function skips over any
-- comments and instructions and concatenates all content until the next start
-- or end tag.
contentMaybe :: MonadThrow m => Consumer Event m (Maybe Text)
contentMaybe = do
    x <- CL.peek
    case pc' x of
        Ignore -> CL.drop 1 >> contentMaybe
        IsContent t -> CL.drop 1 >> fmap Just (takeContents (t:))
        IsError e -> lift $ monadThrow $ XmlException e x
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
            IsError e -> lift $ monadThrow $ XmlException e x
            NotContent -> return $ T.concat $ front []

-- | Grabs the next piece of content. If none if available, returns 'T.empty'.
-- This is simply a wrapper around 'contentMaybe'.
content :: MonadThrow m => Consumer Event m Text
content = fromMaybe T.empty <$> contentMaybe

-- | The most generic way to parse a tag. It takes a predicate for checking if
-- this is the correct tag name, an 'AttrParser' for handling attributes, and
-- then a parser for dealing with content.
--
-- This function automatically absorbs its balancing closing tag, and will
-- throw an exception if not all of the attributes or child elements are
-- consumed. If you want to allow extra attributes, see 'ignoreAttrs'.
--
-- This function automatically ignores comments, instructions and whitespace.
tag :: MonadThrow m
    => (Name -> Maybe a) -- ^ Check if this is a correct tag name
                         --   and return a value that can be used to get an @AttrParser@.
                         --   If this returns @Nothing@, the function will also return @Nothing@
    -> (a -> AttrParser b) -- ^ Given the value returned by the name checker, this function will
                           --   be used to get an @AttrParser@ appropriate for the specific tag.
    -> (b -> CI.ConduitM Event o m c) -- ^ Handler function to handle the attributes and children
                                      --   of a tag, given the value return from the @AttrParser@
    -> CI.ConduitM Event o m (Maybe c)
tag checkName attrParser f = do
    x <- dropWS
    case x of
        Just (EventBeginElement name as) ->
            case checkName name of
                Just y ->
                    case runAttrParser' (attrParser y) as of
                        Left e -> lift $ monadThrow e
                        Right z -> do
                            CL.drop 1
                            z' <- f z
                            a <- dropWS
                            case a of
                                Just (EventEndElement name')
                                    | name == name' -> CL.drop 1 >> return (Just z')
                                _ -> lift $ monadThrow $ XmlException ("Expected end tag for: " ++ show name) a
                Nothing -> return Nothing
        _ -> return Nothing
  where
    -- Drop Events until we encount a non-whitespace element
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
tagPredicate :: MonadThrow m
             => (Name -> Bool) -- ^ Name predicate that returns @True@ if the name matches the parser
             -> AttrParser a -- ^ The attribute parser to be used for tags matching the predicate
             -> (a -> CI.ConduitM Event o m b) -- ^ Handler function to handle the attributes and children
                                               --   of a tag, given the value return from the @AttrParser@
             -> CI.ConduitM Event o m (Maybe b)
tagPredicate p attrParser = tag (guard . p) (const attrParser)

-- | A simplified version of 'tag' which matches for specific tag names instead
-- of taking a predicate function. This is often sufficient, and when combined
-- with OverloadedStrings and the IsString instance of 'Name', can prove to be
-- very concise.
-- .
-- Note that @Name@ is namespace sensitive. When using the @IsString@ instance of name,
-- use
-- > "{http://a/b}c" :: Name
-- to match the tag @c@ in the XML namespace @http://a/b@
tagName :: MonadThrow m
     => Name -- ^ The tag name this parser matches to (includes namespaces)
     -> AttrParser a -- ^ The attribute parser to be used for tags matching the predicate 
     -> (a -> CI.ConduitM Event o m b) -- ^ Handler function to handle the attributes and children
                                       --   of a tag, given the value return from the @AttrParser@
     -> CI.ConduitM Event o m (Maybe b)
tagName name = tagPredicate (== name)

-- | A further simplified tag parser, which requires that no attributes exist.
tagNoAttr :: MonadThrow m
          => Name -- ^ The name this parser matches to
          -> CI.ConduitM Event o m a -- ^ Handler function to handle the children of the matched tag
          -> CI.ConduitM Event o m (Maybe a)
tagNoAttr name f = tagName name (return ()) $ const f

-- | Get the value of the first parser which returns 'Just'. If no parsers
-- succeed (i.e., return @Just@), this function returns 'Nothing'.
--
-- > orE a b = choose [a, b]
orE :: Monad m
    => Consumer Event m (Maybe a) -- ^ The first (preferred) parser
    -> Consumer Event m (Maybe a) -- ^ The second parser, only executed if the first parser fails
    -> Consumer Event m (Maybe a) 
orE a b =
    a >>= \x -> maybe b (const $ return x) x

-- | Get the value of the first parser which returns 'Just'. If no parsers
-- succeed (i.e., return 'Just'), this function returns 'Nothing'.
choose :: Monad m
       => [Consumer Event m (Maybe a)] -- ^ List of parsers that will be tried in order.
       -> Consumer Event m (Maybe a) -- ^ Result of the first parser to succeed, or @Nothing@
                                     --   if no parser succeeded
choose [] = return Nothing
choose (i:is) =
    i >>= maybe (choose is) (return . Just)

-- | Force an optional parser into a required parser. All of the 'tag'
-- functions, 'choose' and 'many' deal with 'Maybe' parsers. Use this when you
-- want to finally force something to happen.
force :: MonadThrow m
      => String -- ^ Error message
      -> CI.ConduitM Event o m (Maybe a) -- ^ Optional parser to be forced
      -> CI.ConduitM Event o m a
force msg i =
    i >>= maybe (lift $ monadThrow $ XmlException msg Nothing) return

-- | A helper function which reads a file from disk using 'enumFile', detects
-- character encoding using 'detectUtf', parses the XML using 'parseBytes', and
-- then hands off control to your supplied parser.
parseFile :: MonadResource m
          => ParseSettings
          -> FilePath
          -> Producer m Event
parseFile ps fp = sourceFile (encodeString fp) =$= parseBytes ps

-- | Parse an event stream from a lazy 'L.ByteString'.
parseLBS :: MonadThrow m
         => ParseSettings
         -> L.ByteString
         -> Producer m Event
parseLBS ps lbs = CL.sourceList (L.toChunks lbs) =$= parseBytes ps

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
        either Left (\(as', f') -> runAttrParser (g f') as') (f as)
instance Functor AttrParser where
    fmap = liftM
instance Applicative AttrParser where
    pure = return
    (<*>) = ap
instance Alternative AttrParser where
    empty = AttrParser $ const $ Left $ XmlException "AttrParser.empty" Nothing
    AttrParser f <|> AttrParser g = AttrParser $ \x ->
        either (const $ g x) Right (f x)

optionalAttrRaw :: ((Name, [Content]) -> Maybe b) -> AttrParser (Maybe b)
optionalAttrRaw f =
    AttrParser $ go id
  where
    go front [] = Right (front [], Nothing)
    go front (a:as) =
        maybe (go (front . (:) a) as)
              (\b -> Right (front as, Just b))
              (f a)

requireAttrRaw :: String -> ((Name, [Content]) -> Maybe b) -> AttrParser b
requireAttrRaw msg f = optionalAttrRaw f >>=
    maybe (AttrParser $ const $ Left $ XmlException msg Nothing)
          return

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
ignoreAttrs = AttrParser $ const $ Right ([], ())

-- | Keep parsing elements as long as the parser returns 'Just'.
many :: Monad m
     => Consumer Event m (Maybe a)
     -> Consumer Event m [a]
many i =
    go id
  where
    go front = i >>=
        maybe (return $ front [])
              (\y -> go $ front . (:) y)

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
            maybe backup ContentText $ Map.lookup t htmlEntities

htmlEntities :: Map.Map T.Text T.Text
htmlEntities = Map.fromList
    $ map (pack *** pack) -- Work around the long-compile-time bug
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
