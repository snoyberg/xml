{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- | This module provides both a native Haskell solution for parsing XML
-- documents into a stream of events, and a set of parser combinators for
-- dealing with a stream of events.
--
-- As a simple example:
--
-- >>> :set -XOverloadedStrings
-- >>> import Conduit (runConduit, (.|))
-- >>> import Data.Text (Text, unpack)
-- >>> import Data.XML.Types (Event)
-- >>> data Person = Person Int Text Text deriving Show
-- >>> :{
-- let parsePerson :: MonadThrow m => ConduitT Event o m (Maybe Person)
--     parsePerson = tag' "person" parseAttributes $ \(age, goodAtHaskell) -> do
--       name <- content
--       return $ Person (read $ unpack age) name goodAtHaskell
--       where parseAttributes = (,) <$> requireAttr "age" <*> requireAttr "goodAtHaskell" <* ignoreAttrs
--     parsePeople :: MonadThrow m => ConduitT Event o m (Maybe [Person])
--     parsePeople = tagNoAttr "people" $ many parsePerson
--     inputXml = mconcat
--       [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
--       , "<people>"
--       , "  <person age=\"25\" goodAtHaskell=\"yes\">Michael</person>"
--       , "  <person age=\"2\" goodAtHaskell=\"might become\">Eliezer</person>"
--       , "</people>"
--       ]
-- :}
--
-- >>> runConduit $ parseLBS def inputXml .| force "people required" parsePeople
-- [Person 25 "Michael" "yes",Person 2 "Eliezer" "might become"]
--
--
-- This module also supports streaming results using 'yield'.
-- This allows parser results to be processed using conduits
-- while a particular parser (e.g. 'many') is still running.
-- Without using streaming results, you have to wait until the parser finished
-- before you can process the result list. Large XML files might be easier
-- to process by using streaming results.
-- See http://stackoverflow.com/q/21367423/2597135 for a related discussion.
--
-- >>> import Data.Conduit.List as CL
-- >>> :{
-- let parsePeople' :: MonadThrow m => ConduitT Event Person m (Maybe ())
--     parsePeople' = tagNoAttr "people" $ manyYield parsePerson
-- :}
--
-- >>> runConduit $ parseLBS def inputXml .| force "people required" parsePeople' .| CL.mapM_ print
-- Person 25 "Michael" "yes"
-- Person 2 "Eliezer" "might become"
--
-- Previous versions of this module contained a number of more sophisticated
-- functions written by Aristid Breitkreuz and Dmitry Olshansky. To keep this
-- package simpler, those functions are being moved to a separate package. This
-- note will be updated with the name of the package(s) when available.
module Text.XML.Stream.Parse
    ( -- * Parsing XML files
      parseBytes
    , parseBytesPos
    , parseText
    , parseTextPos
    , detectUtf
    , parseFile
    , parseLBS
      -- ** Parser settings
    , ParseSettings
    , def
    , DecodeEntities
    , DecodeIllegalCharacters
    , psDecodeEntities
    , psDecodeIllegalCharacters
    , psRetainNamespaces
      -- *** Entity decoding
    , decodeXmlEntities
    , decodeHtmlEntities
      -- * Event parsing
    , tag
    , tag'
    , tagNoAttr
    , tagIgnoreAttrs
    , content
    , contentMaybe
      -- * Ignoring tags/trees
    , ignoreEmptyTag
    , ignoreTree
    , ignoreContent
    , ignoreTreeContent
    , ignoreAnyTreeContent
      -- * Streaming events
    , takeContent
    , takeTree
    , takeTreeContent
    , takeAnyTreeContent
      -- * Tag name matching
    , NameMatcher(..)
    , matching
    , anyOf
    , anyName
      -- * Attribute parsing
    , AttrParser
    , attr
    , requireAttr
    , optionalAttr
    , requireAttrRaw
    , optionalAttrRaw
    , ignoreAttrs
      -- * Combinators
    , orE
    , choose
    , many
    , many_
    , manyIgnore
    , many'
    , force
      -- * Streaming combinators
    , manyYield
    , manyYield'
    , manyIgnoreYield
      -- * Exceptions
    , XmlException (..)
      -- * Other types
    , PositionRange
    , EventPos
    ) where
import           Conduit
import           Control.Applicative          (Alternative (empty, (<|>)),
                                               Applicative (..), (<$>))
import qualified Control.Applicative          as A
import           Control.Arrow                ((***))
import           Control.Exception            (Exception (..), SomeException)
import           Control.Monad                (ap, liftM, void)
import           Control.Monad.Fix            (fix)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow (..),
                                               throwM)
import           Data.Attoparsec.Text         (Parser, anyChar, char, manyTill,
                                               skipWhile, string, takeWhile,
                                               takeWhile1, try)
import qualified Data.Attoparsec.Text         as AT
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Char                    (isSpace)
import           Data.Conduit.Attoparsec      (PositionRange, conduitParser)
import qualified Data.Conduit.Text            as CT
import           Data.Default.Class           (Default (..))
import           Data.List                    (foldl', intercalate)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe, isNothing)
import           Data.String                  (IsString (..))
import           Data.Text                    (Text, pack)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8With)
import           Data.Text.Encoding.Error     (lenientDecode)
import           Data.Typeable                (Typeable)
import           Data.XML.Types               (Content (..), Event (..),
                                               ExternalID (..),
                                               Instruction (..), Name (..))
import           Prelude                      hiding (takeWhile)
import           Text.XML.Stream.Token

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Conduit
-- >>> import Control.Monad (void)

type Ents = [(Text, Text)]

tokenToEvent :: ParseSettings -> Ents -> [NSLevel] -> Token -> (Ents, [NSLevel], [Event])
tokenToEvent _ es n (TokenXMLDeclaration _) = (es, n, [])
tokenToEvent _ es n (TokenInstruction i) = (es, n, [EventInstruction i])
tokenToEvent ps es n (TokenBeginElement name as isClosed _) =
    (es, n', if isClosed then [begin, end] else [begin])
  where
    l0 = case n of
            []  -> NSLevel Nothing Map.empty
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
            []   -> (NSLevel Nothing Map.empty, [])
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
detectUtf :: MonadThrow m => ConduitT S.ByteString T.Text m ()
detectUtf =
    conduit id
  where
    conduit front = await >>= maybe (return ()) (push front)

    push front bss =
        either conduit
               (uncurry checkXMLDecl)
               (getEncoding front bss)

    getEncoding front bs'
        | S.length bs < 4 =
            Left (bs `S.append`)
        | otherwise =
            Right (bsOut, mcodec)
      where
        bs = front bs'
        bsOut = S.append (S.drop toDrop x) y
        (x, y) = S.splitAt 4 bs
        (toDrop, mcodec) =
            case S.unpack x of
                [0x00, 0x00, 0xFE, 0xFF] -> (4, Just CT.utf32_be)
                [0xFF, 0xFE, 0x00, 0x00] -> (4, Just CT.utf32_le)
                0xFE : 0xFF: _           -> (2, Just CT.utf16_be)
                0xFF : 0xFE: _           -> (2, Just CT.utf16_le)
                0xEF : 0xBB: 0xBF : _    -> (3, Just CT.utf8)
                [0x00, 0x00, 0x00, 0x3C] -> (0, Just CT.utf32_be)
                [0x3C, 0x00, 0x00, 0x00] -> (0, Just CT.utf32_le)
                [0x00, 0x3C, 0x00, 0x3F] -> (0, Just CT.utf16_be)
                [0x3C, 0x00, 0x3F, 0x00] -> (0, Just CT.utf16_le)
                _                        -> (0, Nothing) -- Assuming UTF-8

checkXMLDecl :: MonadThrow m
             => S.ByteString
             -> Maybe CT.Codec
             -> ConduitT S.ByteString T.Text m ()
checkXMLDecl bs (Just codec) = leftover bs >> CT.decode codec
checkXMLDecl bs0 Nothing =
    loop [] (AT.parse (parseToken def)) bs0
  where
    loop chunks0 parser nextChunk =
        case parser $ decodeUtf8With lenientDecode nextChunk of
            AT.Fail{} -> fallback
            AT.Partial f -> await >>= maybe fallback (loop chunks f)
            AT.Done _ (TokenXMLDeclaration attrs) -> findEncoding attrs
            AT.Done{} -> fallback
      where
        chunks = nextChunk : chunks0
        fallback = complete CT.utf8
        complete codec = mapM_ leftover chunks >> CT.decode codec

        findEncoding [] = fallback
        findEncoding ((TName _ "encoding", [ContentText enc]):_) =
            case T.toLower enc of
                "iso-8859-1" -> complete CT.iso8859_1
                "utf-8"      -> complete CT.utf8
                _            -> complete CT.utf8
        findEncoding (_:xs) = findEncoding xs

type EventPos = (Maybe PositionRange, Event)

-- | Parses a byte stream into 'Event's. This function is implemented fully in
-- Haskell using attoparsec-text for parsing. The produced error messages do
-- not give line/column information, so you may prefer to stick with the parser
-- provided by libxml-enumerator. However, this has the advantage of not
-- relying on any C libraries.
--
-- This relies on 'detectUtf' to determine character encoding, and 'parseText'
-- to do the actual parsing.
parseBytes :: MonadThrow m
           => ParseSettings
           -> ConduitT S.ByteString Event m ()
parseBytes = mapOutput snd . parseBytesPos

parseBytesPos :: MonadThrow m
              => ParseSettings
              -> ConduitT S.ByteString EventPos m ()
parseBytesPos ps = detectUtf .| parseTextPos ps

dropBOM :: Monad m => ConduitT T.Text T.Text m ()
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
--
-- Since 1.2.4
parseText :: MonadThrow m => ParseSettings -> ConduitT T.Text Event m ()
parseText = mapOutput snd . parseTextPos


-- | Same as 'parseText', but includes the position of each event.
--
-- Since 1.2.4
parseTextPos :: MonadThrow m
          => ParseSettings
          -> ConduitT T.Text EventPos m ()
parseTextPos de =
    dropBOM
        .| tokenize
        .| toEventC de
        .| addBeginEnd
  where
    tokenize = conduitToken de
    addBeginEnd = yield (Nothing, EventBeginDocument) >> addEnd
    addEnd = await >>= maybe
        (yield (Nothing, EventEndDocument))
        (\e -> yield e >> addEnd)

toEventC :: Monad m => ParseSettings -> ConduitT (PositionRange, Token) EventPos m ()
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


type DecodeEntities = Text -> Content
type DecodeIllegalCharacters = Int -> Maybe Char

data ParseSettings = ParseSettings
    { psDecodeEntities          :: DecodeEntities
    , psRetainNamespaces        :: Bool
    -- ^ Whether the original xmlns attributes should be retained in the parsed
    -- values. For more information on motivation, see:
    --
    -- <https://github.com/snoyberg/xml/issues/38>
    --
    -- Default: False
    --
    -- Since 1.2.1
    , psDecodeIllegalCharacters :: DecodeIllegalCharacters
    -- ^ How to decode illegal character references (@&#[0-9]+;@ or @&#x[0-9a-fA-F]+;@).
    --
    -- Character references within the legal ranges defined by <https://www.w3.org/TR/REC-xml/#NT-Char the standard> are automatically parsed.
    -- Others are passed to this function.
    --
    -- Default: @const Nothing@
    --
    -- Since 1.7.1
    }

instance Default ParseSettings where
    def = ParseSettings
        { psDecodeEntities = decodeXmlEntities
        , psRetainNamespaces = False
        , psDecodeIllegalCharacters = const Nothing
        }

conduitToken :: MonadThrow m => ParseSettings -> ConduitT T.Text (PositionRange, Token) m ()
conduitToken = conduitParser . parseToken

parseToken :: ParseSettings -> Parser Token
parseToken settings = (char '<' >> parseLt) <|> TokenContent <$> parseContent settings False False
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
                as <- A.many $ parseAttribute settings
                skipSpace
                char' '?'
                char' '>'
                newline <|> return ()
                return $ TokenXMLDeclaration as
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
                    TName Nothing x  -> x
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
    parsePublicID = PublicID <$> (string "PUBLIC" *> quotedText) <*> quotedText
    parseSystemID = SystemID <$> (string "SYSTEM" *> quotedText)
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
        as <- A.many $ parseAttribute settings
        skipSpace
        isClose <- (char '/' >> skipSpace >> return True) <|> return False
        char' '>'
        return $ TokenBeginElement n as isClose 0

parseAttribute :: ParseSettings -> Parser TAttribute
parseAttribute settings = do
    skipSpace
    key <- parseName
    skipSpace
    char' '='
    skipSpace
    val <- squoted <|> dquoted
    return (key, val)
  where
    squoted = char '\'' *> manyTill (parseContent settings False True) (char '\'')
    dquoted = char  '"' *> manyTill (parseContent settings True False) (char  '"')

parseName :: Parser TName
parseName =
  name <$> parseIdent <*> A.optional (char ':' >> parseIdent)
  where
    name i1 Nothing   = TName Nothing i1
    name i1 (Just i2) = TName (Just i1) i2

parseIdent :: Parser Text
parseIdent =
    takeWhile1 valid
  where
    valid '&'  = False
    valid '<'  = False
    valid '>'  = False
    valid ':'  = False
    valid '?'  = False
    valid '='  = False
    valid '"'  = False
    valid '\'' = False
    valid '/'  = False
    valid ';'  = False
    valid '#'  = False
    valid c    = not $ isXMLSpace c

parseContent :: ParseSettings
             -> Bool -- break on double quote
             -> Bool -- break on single quote
             -> Parser Content
parseContent (ParseSettings decodeEntities _ decodeIllegalCharacters) breakDouble breakSingle = parseReference <|> parseTextContent where
  parseReference = do
    char' '&'
    t <- parseEntityRef <|> parseHexCharRef <|> parseDecCharRef
    char' ';'
    return t
  parseEntityRef = do
    TName ma b <- parseName
    let name = maybe "" (`T.append` ":") ma `T.append` b
    return $ case name of
      "lt"   -> ContentText "<"
      "gt"   -> ContentText ">"
      "amp"  -> ContentText "&"
      "quot" -> ContentText "\""
      "apos" -> ContentText "'"
      _      -> decodeEntities name
  parseHexCharRef = do
    void $ string "#x"
    n <- AT.hexadecimal
    case toValidXmlChar n <|> decodeIllegalCharacters n of
      Nothing -> fail "Invalid character from hexadecimal character reference."
      Just c -> return $ ContentText $ T.singleton c
  parseDecCharRef = do
    void $ string "#"
    n <- AT.decimal
    case toValidXmlChar n <|> decodeIllegalCharacters n of
      Nothing -> fail "Invalid character from decimal character reference."
      Just c  -> return $ ContentText $ T.singleton c
  parseTextContent = ContentText <$> takeWhile1 valid
  valid '"'  = not breakDouble
  valid '\'' = not breakSingle
  valid '&'  = False -- amp
  valid '<'  = False -- lt
  valid _    = True

-- | Is this codepoint a valid XML character? See
-- <https://www.w3.org/TR/xml/#charsets>. This is proudly XML 1.0 only.
toValidXmlChar :: Int -> Maybe Char
toValidXmlChar n
  | any checkRange ranges = Just (toEnum n)
  | otherwise = Nothing
  where
    --Inclusive lower bound, inclusive upper bound.
    ranges :: [(Int, Int)]
    ranges =
      [ (0x9, 0xA)
      , (0xD, 0xD)
      , (0x20, 0xD7FF)
      , (0xE000, 0xFFFD)
      , (0x10000, 0x10FFFF)
      ]
    checkRange (lb, ub) = lb <= n && n <= ub

skipSpace :: Parser ()
skipSpace = skipWhile isXMLSpace

-- | Determines whether a character is an XML white space. The list of
-- white spaces is given by
--
-- >  S ::= (#x20 | #x9 | #xD | #xA)+
--
-- in <http://www.w3.org/TR/2008/REC-xml-20081126/#sec-common-syn>.
isXMLSpace :: Char -> Bool
isXMLSpace ' '  = True
isXMLSpace '\t' = True
isXMLSpace '\r' = True
isXMLSpace '\n' = True
isXMLSpace _    = False

newline :: Parser ()
newline = void $ (char '\r' >> char '\n') <|> char '\n'

char' :: Char -> Parser ()
char' = void . char

data ContentType = Ignore | IsContent Text | IsError String | NotContent

-- | Grabs the next piece of content if available. This function skips over any
-- comments, instructions or entities, and concatenates all content until the next start
-- or end tag.
contentMaybe :: MonadThrow m => ConduitT Event o m (Maybe Text)
contentMaybe = do
    x <- peekC
    case pc' x of
        Ignore      -> dropC 1 >> contentMaybe
        IsContent t -> dropC 1 >> fmap Just (takeContents (t:))
        IsError e   -> lift $ throwM $ InvalidEntity e x
        NotContent  -> return Nothing
  where
    pc' Nothing  = NotContent
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
        x <- peekC
        case pc' x of
            Ignore      -> dropC 1 >> takeContents front
            IsContent t -> dropC 1 >> takeContents (front . (:) t)
            IsError e   -> lift $ throwM $ InvalidEntity e x
            NotContent  -> return $ T.concat $ front []

-- | Grabs the next piece of content. If none if available, returns 'T.empty'.
-- This is simply a wrapper around 'contentMaybe'.
content :: MonadThrow m => ConduitT Event o m Text
content = fromMaybe T.empty <$> contentMaybe


isWhitespace :: Event -> Bool
isWhitespace EventBeginDocument             = True
isWhitespace EventEndDocument               = True
isWhitespace EventBeginDoctype{}            = True
isWhitespace EventEndDoctype                = True
isWhitespace EventInstruction{}             = True
isWhitespace (EventContent (ContentText t)) = T.all isSpace t
isWhitespace EventComment{}                 = True
isWhitespace (EventCDATA t)                 = T.all isSpace t
isWhitespace _                              = False


-- | The most generic way to parse a tag. It takes a 'NameMatcher' to check whether
-- this is a correct tag name, an 'AttrParser' to handle attributes, and
-- then a parser to deal with content.
--
-- 'Events' are consumed if and only if the tag name and its attributes match.
--
-- This function automatically absorbs its balancing closing tag, and will
-- throw an exception if not all of the attributes or child elements are
-- consumed. If you want to allow extra attributes, see 'ignoreAttrs'.
--
-- This function automatically ignores comments, instructions and whitespace.
tag :: MonadThrow m
    => NameMatcher a -- ^ Check if this is a correct tag name
                     --   and return a value that can be used to get an @AttrParser@.
                     --   If this fails, the function will return @Nothing@
    -> (a -> AttrParser b) -- ^ Given the value returned by the name checker, this function will
                           --   be used to get an @AttrParser@ appropriate for the specific tag.
                           --   If the @AttrParser@ fails, the function will also return @Nothing@
    -> (b -> ConduitT Event o m c) -- ^ Handler function to handle the attributes and children
                                   --   of a tag, given the value return from the @AttrParser@
    -> ConduitT Event o m (Maybe c)
tag nameMatcher attrParser f = do
  (x, leftovers) <- dropWS []
  res <- case x of
    Just (EventBeginElement name as) -> case runNameMatcher nameMatcher name of
      Just y -> case runAttrParser' (attrParser y) as of
        Left _ -> return Nothing
        Right z -> do
          z' <- f z
          (a, _leftovers') <- dropWS []
          case a of
            Just (EventEndElement name')
              | name == name' -> return (Just z')
            _ -> lift $ throwM $ InvalidEndElement name a
      Nothing -> return Nothing
    _ -> return Nothing

  case res of
    -- Did not parse, put back all of the leading whitespace events and the
    -- final observed event generated by dropWS
    Nothing -> mapM_ leftover leftovers
    -- Parse succeeded, discard all of those whitespace events and the
    -- first parsed event
    Just _  -> return ()

  return res
  where
    -- Drop Events until we encounter a non-whitespace element. Return all of
    -- the events consumed here (including the first non-whitespace event) so
    -- that the calling function can treat them as leftovers if the parse fails
    dropWS leftovers = do
        x <- await
        let leftovers' = maybe id (:) x leftovers

        case isWhitespace <$> x of
          Just True -> dropWS leftovers'
          _         -> return (x, leftovers')
    runAttrParser' p as =
        case runAttrParser p as of
            Left e           -> Left e
            Right ([], x)    -> Right x
            Right (attr', _) -> Left $ toException $ UnparsedAttributes attr'

-- | A simplified version of 'tag' where the 'NameMatcher' result isn't forwarded to the attributes parser.
--
-- Since 1.5.0
tag' :: MonadThrow m
     => NameMatcher a -> AttrParser b -> (b -> ConduitT Event o m c)
     -> ConduitT Event o m (Maybe c)
tag' a b = tag a (const b)

-- | A further simplified tag parser, which requires that no attributes exist.
tagNoAttr :: MonadThrow m
          => NameMatcher a -- ^ Check if this is a correct tag name
          -> ConduitT Event o m b -- ^ Handler function to handle the children of the matched tag
          -> ConduitT Event o m (Maybe b)
tagNoAttr name f = tag' name (return ()) $ const f


-- | A further simplified tag parser, which ignores all attributes, if any exist
tagIgnoreAttrs :: MonadThrow m
               => NameMatcher a -- ^ Check if this is a correct tag name
               -> ConduitT Event o m b -- ^ Handler function to handle the children of the matched tag
               -> ConduitT Event o m (Maybe b)
tagIgnoreAttrs name f = tag' name ignoreAttrs $ const f


-- | Ignore an empty tag and all of its attributes.
--   This does not ignore the tag recursively
--   (i.e. it assumes there are no child elements).
--   This function returns @Just ()@ if the tag matched.
--
-- Since 1.5.0
ignoreEmptyTag :: MonadThrow m
          => NameMatcher a -- ^ Check if this is a correct tag name
          -> ConduitT Event o m (Maybe ())
ignoreEmptyTag nameMatcher = tagIgnoreAttrs nameMatcher (return ())


ignored :: Monad m => ConduitT i o m ()
ignored = fix $ \recurse -> do
  event <- await
  case event of
    Just _ -> recurse
    _      -> return ()


-- | Same as `takeTree`, without yielding `Event`s.
--
-- >>> runConduit $ parseLBS def "<a>content</a><b></b>" .| (ignoreTree "a" ignoreAttrs >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "b", ...}) [],EventEndElement (Name {nameLocalName = "b", ...}),EventEndDocument]
--
-- >>> runConduit $ parseLBS def "<a>content</a>" .| (ignoreTree "b" ignoreAttrs >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "a", ...}) [],EventContent (ContentText "content"),EventEndElement (Name {nameLocalName = "a", ...}),EventEndDocument]
--
-- >>> runConduit $ parseLBS def "content<a></a>" .| (ignoreTree anyName ignoreAttrs >> sinkList)
-- [EventContent (ContentText "content"),EventBeginElement (Name {nameLocalName = "a", ...}) [],EventEndElement (Name {nameLocalName = "a", ...}),EventEndDocument]
--
-- Since 1.9.0
ignoreTree :: MonadThrow m => NameMatcher a -> AttrParser b -> ConduitT Event o m (Maybe ())
ignoreTree nameMatcher attrParser = fuseUpstream (takeTree nameMatcher attrParser) ignored

-- | Same as `takeContent`, without yielding `Event`s.
--
-- >>> runConduit $ parseLBS def "<a>content</a>" .| (ignoreContent >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "a", ...}) [],EventContent (ContentText "content"),EventEndElement (Name {nameLocalName = "a", ...}),EventEndDocument]
--
-- >>> runConduit $ parseLBS def "content<a></a>" .| (ignoreContent >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "a", ...}) [],EventEndElement (Name {nameLocalName = "a", ...}),EventEndDocument]
--
-- >>> runConduit $ parseLBS def "content<a></a>" .| (ignoreContent >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "a", ...}) [],EventEndElement (Name {nameLocalName = "a", ...}),EventEndDocument]
--
-- Since 1.9.0
ignoreContent :: MonadThrow m => ConduitT Event o m (Maybe ())
ignoreContent = fuseUpstream takeContent ignored


-- | Same as `takeTreeContent`, without yielding `Event`s.
--
-- >>> runConduit $ parseLBS def "<a>content</a><b></b>" .| (ignoreTreeContent "a" ignoreAttrs >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "b", ...}) [],EventEndElement (Name {nameLocalName = "b", ...}),EventEndDocument]
--
-- >>> runConduit $ parseLBS def "<a>content</a>" .| (ignoreTreeContent "b" ignoreAttrs >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "a", ...}) [],EventContent (ContentText "content"),EventEndElement (Name {nameLocalName = "a", ...}),EventEndDocument]
--
-- >>> runConduit $ parseLBS def "content<a></a>" .| (ignoreTreeContent anyName ignoreAttrs >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "a", ...}) [],EventEndElement (Name {nameLocalName = "a", ...}),EventEndDocument]
--
-- Since 1.5.0
ignoreTreeContent :: MonadThrow m => NameMatcher a -> AttrParser b -> ConduitT Event o m (Maybe ())
ignoreTreeContent namePred attrParser = fuseUpstream (takeTreeContent namePred attrParser) ignored


-- | Same as `takeAnyTreeContent`, without yielding `Event`s.
--
-- >>> runConduit $ parseLBS def "<a>content</a><b></b>" .| (ignoreAnyTreeContent >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "b", ...}) [],EventEndElement (Name {nameLocalName = "b", ...}),EventEndDocument]
--
-- >>> runConduit $ parseLBS def "text<b></b>" .| (ignoreAnyTreeContent >> sinkList)
-- [EventBeginElement (Name {nameLocalName = "b", ...}) [],EventEndElement (Name {nameLocalName = "b", ...}),EventEndDocument]
--
-- Since 1.5.0
ignoreAnyTreeContent :: MonadThrow m => ConduitT Event o m (Maybe ())
ignoreAnyTreeContent = fuseUpstream takeAnyTreeContent ignored


-- | Get the value of the first parser which returns 'Just'. If no parsers
-- succeed (i.e., return @Just@), this function returns 'Nothing'.
--
-- > orE a b = choose [a, b]
orE :: Monad m
    => ConduitT Event o m (Maybe a) -- ^ The first (preferred) parser
    -> ConduitT Event o m (Maybe a) -- ^ The second parser, only executed if the first parser fails
    -> ConduitT Event o m (Maybe a)
orE a b = a >>= \x -> maybe b (const $ return x) x

-- | Get the value of the first parser which returns 'Just'. If no parsers
-- succeed (i.e., return 'Just'), this function returns 'Nothing'.
choose :: Monad m
       => [ConduitT Event o m (Maybe a)] -- ^ List of parsers that will be tried in order.
       -> ConduitT Event o m (Maybe a)   -- ^ Result of the first parser to succeed, or @Nothing@
                                         --   if no parser succeeded
choose []     = return Nothing
choose (i:is) = i >>= maybe (choose is) (return . Just)

-- | Force an optional parser into a required parser. All of the 'tag'
-- functions, 'attr', 'choose' and 'many' deal with 'Maybe' parsers. Use this when you
-- want to finally force something to happen.
force :: MonadThrow m
      => String -- ^ Error message
      -> m (Maybe a) -- ^ Optional parser to be forced
      -> m a
force msg i = i >>= maybe (throwM $ XmlException msg Nothing) return

-- | A helper function which reads a file from disk using 'enumFile', detects
-- character encoding using 'detectUtf', parses the XML using 'parseBytes', and
-- then hands off control to your supplied parser.
parseFile :: MonadResource m
          => ParseSettings
          -> FilePath
          -> ConduitT i Event m ()
parseFile ps fp = sourceFile fp .| transPipe liftIO (parseBytes ps)

-- | Parse an event stream from a lazy 'L.ByteString'.
parseLBS :: MonadThrow m
         => ParseSettings
         -> L.ByteString
         -> ConduitT i Event m ()
parseLBS ps lbs = sourceLazy lbs .| parseBytes ps

data XmlException = XmlException
    { xmlErrorMessage :: String
    , xmlBadInput     :: Maybe Event
    }
                  | InvalidEndElement Name (Maybe Event)
                  | InvalidEntity String (Maybe Event)
                  | MissingAttribute String
                  | UnparsedAttributes [(Name, [Content])]
    deriving (Show, Typeable)

instance Exception XmlException where
#if MIN_VERSION_base(4, 8, 0)
  displayException (XmlException msg (Just event)) = "Error while parsing XML event " ++ show event ++ ": " ++ msg
  displayException (XmlException msg _) = "Error while parsing XML: " ++ msg
  displayException (InvalidEndElement name (Just event)) = "Error while parsing XML event: expected </" ++ T.unpack (nameLocalName name) ++ ">, got " ++ show event
  displayException (InvalidEndElement name _) = "Error while parsing XML event: expected </" ++ show name ++ ">, got nothing"
  displayException (InvalidEntity msg (Just event)) = "Error while parsing XML entity " ++ show event ++ ": " ++ msg
  displayException (InvalidEntity msg _) = "Error while parsing XML entity: " ++ msg
  displayException (MissingAttribute msg) = "Missing required attribute: " ++ msg
  displayException (UnparsedAttributes attrs) = show (length attrs) ++ " remaining unparsed attributes: \n" ++ intercalate "\n" (show <$> attrs)
#endif


-- | A @NameMatcher@ describes which names a tag parser is allowed to match.
--
-- Since 1.5.0
newtype NameMatcher a = NameMatcher { runNameMatcher :: Name -> Maybe a }

deriving instance Functor NameMatcher

instance Applicative NameMatcher where
  pure a = NameMatcher $ const $ pure a
  NameMatcher f <*> NameMatcher a = NameMatcher $ \name -> f name <*> a name

-- | 'NameMatcher's can be combined with @\<|\>@
instance Alternative NameMatcher where
  empty = NameMatcher $ const Nothing
  NameMatcher f <|> NameMatcher g = NameMatcher (\a -> f a <|> g a)

-- | Match a single 'Name' in a concise way.
-- Note that 'Name' is namespace sensitive: when using the 'IsString' instance,
-- use @"{http:\/\/a\/b}c"@ to match the tag @c@ in the XML namespace @http://a/b@
instance (a ~ Name) => IsString (NameMatcher a) where
  fromString s = matching (== fromString s)

-- | @matching f@ matches @name@ iff @f name@ is true. Returns the matched 'Name'.
--
-- Since 1.5.0
matching :: (Name -> Bool) -> NameMatcher Name
matching f = NameMatcher $ \name -> if f name then Just name else Nothing

-- | Matches any 'Name'. Returns the matched 'Name'.
--
-- Since 1.5.0
anyName :: NameMatcher Name
anyName = matching (const True)

-- | Matches any 'Name' from the given list. Returns the matched 'Name'.
--
-- Since 1.5.0
anyOf :: [Name] -> NameMatcher Name
anyOf values = matching (`elem` values)


-- | A monad for parsing attributes. By default, it requires you to deal with
-- all attributes present on an element, and will throw an exception if there
-- are unhandled attributes. Use the 'requireAttr', 'attr' et al
-- functions for handling an attribute, and 'ignoreAttrs' if you would like to
-- skip the rest of the attributes on an element.
--
-- 'Alternative' instance behaves like 'First' monoid: it chooses first
-- parser which doesn't fail.
newtype AttrParser a = AttrParser { runAttrParser :: [(Name, [Content])] -> Either SomeException ([(Name, [Content])], a) }

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
    empty = AttrParser $ const $ Left $ toException $ XmlException "AttrParser.empty" Nothing
    AttrParser f <|> AttrParser g = AttrParser $ \x ->
        either (const $ g x) Right (f x)
instance MonadThrow AttrParser where
    throwM = AttrParser . const . throwM

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
    maybe (AttrParser $ const $ Left $ toException $ MissingAttribute msg)
          return

-- | Return the value for an attribute if present.
attr :: Name -> AttrParser (Maybe Text)
attr n = optionalAttrRaw
    (\(x, y) -> if x == n then Just (contentsToText y) else Nothing)

-- | Shortcut composition of 'force' and 'attr'.
requireAttr :: Name -> AttrParser Text
requireAttr n = force ("Missing attribute: " ++ show n) $ attr n


{-# DEPRECATED optionalAttr "Please use 'attr'." #-}
optionalAttr :: Name -> AttrParser (Maybe Text)
optionalAttr = attr

contentsToText :: [Content] -> Text
contentsToText = T.concat . map toText where
  toText (ContentText t)   = t
  toText (ContentEntity e) = T.concat ["&", e, ";"]

-- | Skip the remaining attributes on an element. Since this will clear the
-- list of attributes, you must call this /after/ any calls to 'requireAttr',
-- 'optionalAttr', etc.
ignoreAttrs :: AttrParser ()
ignoreAttrs = AttrParser $ const $ Right ([], ())

-- | Keep parsing elements as long as the parser returns 'Just'.
many :: Monad m
     => ConduitT Event o m (Maybe a)
     -> ConduitT Event o m [a]
many i = manyIgnore i $ return Nothing

-- | Like 'many' but discards the results without building an intermediate list.
--
-- Since 1.5.0
many_ :: MonadThrow m
      => ConduitT Event o m (Maybe a)
      -> ConduitT Event o m ()
many_ consumer = manyIgnoreYield (return Nothing) (void <$> consumer)

-- | Keep parsing elements as long as the parser returns 'Just'
--   or the ignore parser returns 'Just'.
manyIgnore :: Monad m
           => ConduitT Event o m (Maybe a)
           -> ConduitT Event o m (Maybe b)
           -> ConduitT Event o m [a]
manyIgnore i ignored = go id where
  go front = i >>= maybe (onFail front) (\y -> go $ front . (:) y)
  -- onFail is called if the main parser fails
  onFail front = ignored >>= maybe (return $ front []) (const $ go front)

-- | Like @many@, but any tags and content the consumer doesn't match on
--   are silently ignored.
many' :: MonadThrow m
      => ConduitT Event o m (Maybe a)
      -> ConduitT Event o m [a]
many' consumer = manyIgnore consumer ignoreAnyTreeContent


-- | Like 'many', but uses 'yield' so the result list can be streamed
--   to downstream conduits without waiting for 'manyYield' to finish
manyYield :: Monad m
          => ConduitT a b m (Maybe b)
          -> ConduitT a b m ()
manyYield consumer = fix $ \loop ->
  consumer >>= maybe (return ()) (\x -> yield x >> loop)

-- | Like 'manyIgnore', but uses 'yield' so the result list can be streamed
--   to downstream conduits without waiting for 'manyIgnoreYield' to finish
manyIgnoreYield :: MonadThrow m
                => ConduitT Event b m (Maybe b) -- ^ Consuming parser that generates the result stream
                -> ConduitT Event b m (Maybe ()) -- ^ Ignore parser that consumes elements to be ignored
                -> ConduitT Event b m ()
manyIgnoreYield consumer ignoreParser = fix $ \loop ->
  consumer >>= maybe (onFail loop) (\x -> yield x >> loop)
  where onFail loop = ignoreParser >>= maybe (return ()) (const loop)

-- | Like 'many'', but uses 'yield' so the result list can be streamed
--   to downstream conduits without waiting for 'manyYield'' to finish
manyYield' :: MonadThrow m
           => ConduitT Event b m (Maybe b)
           -> ConduitT Event b m ()
manyYield' consumer = manyIgnoreYield consumer ignoreAnyTreeContent


-- | Stream a single content 'Event'.
--
-- Returns @Just ()@ if a content 'Event' was consumed, @Nothing@ otherwise.
--
-- >>> runConduit $ parseLBS def "content<a></a>" .| void takeContent .| sinkList
-- [EventBeginDocument,EventContent (ContentText "content")]
--
-- If next event isn't a content, nothing is consumed.
--
-- >>> runConduit $ parseLBS def "<a>content</a>" .| void takeContent .| sinkList
-- [EventBeginDocument]
--
-- Since 1.5.0
takeContent :: MonadThrow m => ConduitT Event Event m (Maybe ())
takeContent = do
  event <- await
  case event of
    Just e@EventContent{} -> yield e >> return (Just ())
    Just e@EventCDATA{}   -> yield e >> return (Just ())
    Just e -> if isWhitespace e then yield e >> takeContent else leftover e >> return Nothing
    _ -> return Nothing

-- | Stream 'Event's corresponding to a single XML element that matches given 'NameMatcher' and 'AttrParser', from the opening- to the closing-tag.
--
-- >>> runConduit $ parseLBS def "<a>content</a><b></b>" .| void (takeTree "a" ignoreAttrs) .| sinkList
-- [EventBeginDocument,EventBeginElement (Name {nameLocalName = "a", ...}) [],EventContent (ContentText "content"),EventEndElement (Name {nameLocalName = "a", ...})]
--
-- >>> runConduit $ parseLBS def "<a>content</a><b></b>" .| void (takeTree "b" ignoreAttrs) .| sinkList
-- [EventBeginDocument]
--
-- If next 'Event' isn't an element, nothing is consumed.
--
-- >>> runConduit $ parseLBS def "text<a></a>" .| void (takeTree "a" ignoreAttrs) .| sinkList
-- [EventBeginDocument]
--
-- If an opening-tag is consumed but no matching closing-tag is found, an 'XmlException' is thrown.
--
-- >>> runConduit $ parseLBS def "<a><b></b>" .| void (takeTree "a" ignoreAttrs) .| sinkList
-- *** Exception: InvalidEndElement (Name {nameLocalName = "a", nameNamespace = Nothing, namePrefix = Nothing}) Nothing
--
-- This function automatically ignores comments, instructions and whitespace.
--
-- Returns @Just ()@ if an element was consumed, 'Nothing' otherwise.
--
-- Since 1.5.0
takeTree :: MonadThrow m => NameMatcher a -> AttrParser b -> ConduitT Event Event m (Maybe ())
takeTree nameMatcher attrParser = do
  event <- await
  case event of
    Just e@(EventBeginElement name as) -> case runNameMatcher nameMatcher name of
      Just _ -> case runAttrParser attrParser as of
        Right _ -> do
          yield e
          whileJust takeAnyTreeContent
          endEvent <- await
          case endEvent of
            Just e'@(EventEndElement name') | name == name' -> yield e' >> return (Just ())
            _ -> lift $ throwM $ InvalidEndElement name endEvent
        _ -> leftover e >> return Nothing
      _ -> leftover e >> return Nothing

    Just e -> if isWhitespace e then yield e >> takeTree nameMatcher attrParser else leftover e >> return Nothing
    _ -> return Nothing
  where
    whileJust f = fix $ \loop -> f >>= maybe (return ()) (const loop)

-- | Like 'takeTree', but can also stream a content 'Event'.
--
-- >>> runConduit $ parseLBS def "<a>content</a><b></b>" .| void (takeTreeContent "a" ignoreAttrs) .| sinkList
-- [EventBeginDocument,EventBeginElement (Name {nameLocalName = "a", ...}) [],EventContent (ContentText "content"),EventEndElement (Name {nameLocalName = "a", ...})]
--
-- >>> runConduit $ parseLBS def "<a>content</a><b></b>" .| void (takeTreeContent "b" ignoreAttrs) .| sinkList
-- [EventBeginDocument]
--
-- >>> runConduit $ parseLBS def "content<a></a><b></b>" .| void (takeTreeContent "a" ignoreAttrs) .| sinkList
-- [EventBeginDocument,EventContent (ContentText "content")]
--
-- Since 1.5.0
takeTreeContent :: MonadThrow m => NameMatcher a -> AttrParser b -> ConduitT Event Event m (Maybe ())
takeTreeContent nameMatcher attrParser = runMaybeT $ MaybeT (takeTree nameMatcher attrParser) <|> MaybeT takeContent

-- | Like 'takeTreeContent', without checking for tag name or attributes.
--
-- >>> runConduit $ parseLBS def "text<a></a>" .| void takeAnyTreeContent .| sinkList
-- [EventBeginDocument,EventContent (ContentText "text")]
--
-- >>> runConduit $ parseLBS def "</a><b></b>" .| void takeAnyTreeContent .| sinkList
-- [EventBeginDocument]
--
-- >>> runConduit $ parseLBS def "<b><c></c></b></a>text" .| void takeAnyTreeContent .| sinkList
-- [EventBeginDocument,EventBeginElement (Name {nameLocalName = "b", ...}) [],EventBeginElement (Name {nameLocalName = "c", ...}) [],EventEndElement (Name {nameLocalName = "c", ...}),EventEndElement (Name {nameLocalName = "b", ...})]
--
-- Since 1.5.0
takeAnyTreeContent :: MonadThrow m
                => ConduitT Event Event m (Maybe ())
takeAnyTreeContent = takeTreeContent anyName ignoreAttrs


-- | Default implementation of 'DecodeEntities', which leaves the
-- entity as-is. Numeric character references and the five standard
-- entities (lt, gt, amp, quot, pos) are handled internally by the
-- parser.
decodeXmlEntities :: DecodeEntities
decodeXmlEntities = ContentEntity

-- | HTML4-compliant entity decoder. Handles the additional 248
-- entities defined by HTML 4 and XHTML 1.
--
-- Note that HTML 5 introduces a drastically larger number of entities, and
-- this code does not recognize most of them.
decodeHtmlEntities :: DecodeEntities
decodeHtmlEntities t =
  maybe (ContentEntity t) ContentText $ Map.lookup t htmlEntities

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
