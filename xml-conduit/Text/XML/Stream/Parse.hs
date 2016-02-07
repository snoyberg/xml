{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE RankNTypes         #-}
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
-- This module also supports streaming results using 'yield'.
-- This allows parser results to be processed using conduits
-- while a particular parser (e.g. 'many') is still running.
-- Without using streaming results, you have to wait until the parser finished
-- before you can process the result list. Large XML files might be easier
-- to process by using streaming results.
-- See http://stackoverflow.com/q/21367423/2597135 for a related discussion.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Control.Monad.Trans.Resource
-- > import Data.Conduit
-- > import Data.Text (Text, unpack)
-- > import Text.XML.Stream.Parse
-- > import Text.XML (Name)
-- > import Control.Monad.Trans.Class (lift)
-- > import Control.Monad (void)
-- > import qualified Data.Conduit.List as CL
-- >
-- > data Person = Person Int Text deriving Show
-- >
-- > parsePerson = tagName "person" (requireAttr "age") $ \age -> do
-- >     name <- content
-- >     return $ Person (read $ unpack age) name
-- >
-- > parsePeople = void $ tagNoAttr "people" $ manyYield parsePerson
-- >
-- > main = runResourceT $
-- >     parseFile def "people.xml" $$ parsePeople =$ CL.mapM_ (lift . print)
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
    , psPreserveWhiteSpace
      -- *** Entity decoding
    , decodeXmlEntities
    , decodeHtmlEntities
      -- * Event parsing
    , tag
    , tagPredicate
    , tagName
    , tagNoAttr
    , tagIgnoreAttrs
    , tagPredicateIgnoreAttrs
    , content
    , contentMaybe
      -- * Ignoring tags/trees
    , ignoreTag
    , ignoreTagName
    , ignoreAnyTagName
    , ignoreAllTags
    , ignoreTree
    , ignoreTreeName
    , ignoreAnyTreeName
    , ignoreAllTrees
    , ignoreAllTreesContent
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
    , manyIgnore
    , many'
    , force
      -- * Streaming combinators
    , manyYield
    , manyIgnoreYield
    , manyYield'
      -- * Exceptions
    , XmlException (..)
      -- * Other types
    , PositionRange
    , EventPos
    ) where
import qualified Control.Applicative          as A
import           Control.Monad                (mzero)
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow (..),
                                               monadThrow)
import           Data.Attoparsec.Text         (Parser, anyChar, char, endOfLine, manyTill,
                                               satisfy, skipWhile, string, takeWhile,
                                               takeWhile1, (<?>))
import qualified Data.Attoparsec.Text         as AT
import qualified Data.Attoparsec.Combinator   as AC
import           Data.Conduit.Attoparsec      (PositionRange, conduitParser)
import           Data.XML.Types               (Content (..), Event (..),
                                               ExternalID (..),
                                               Instruction (..), Name (..))

import           Blaze.ByteString.Builder     (fromWord32be, toByteString)
import           Control.Applicative          (Alternative (empty, (<|>)),
                                               Applicative (..), (<$>))
import           Control.Arrow                ((***), (&&&))
import           Control.Exception            (Exception (..), SomeException)
import           Control.Monad                (ap, guard, liftM, void)
import           Control.Monad.Trans.Class    (lift)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit
import           Data.Conduit.Binary          (sourceFile)
import qualified Data.Conduit.Internal        as CI
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Default                 (Default (..))
import           Data.List                    (foldl')
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe, isNothing)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text, pack)
import qualified Data.Text                    as T
import qualified Data.Text                    as TS
import           Data.Text.Encoding           (decodeUtf32BEWith, decodeUtf8With)
import           Data.Text.Encoding.Error     (ignore, lenientDecode)
import           Data.Text.Read               (Reader, decimal, hexadecimal)
import           Data.Typeable                (Typeable)
import           Data.Word                    (Word32)
import           Prelude                      hiding (takeWhile)
import           Text.XML.Stream.Token

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
                [0x00, 0x00, 0xFE, 0xFF] -> (4, Just $ CT.utf32_be)
                [0xFF, 0xFE, 0x00, 0x00] -> (4, Just $ CT.utf32_le)
                0xFE : 0xFF: _           -> (2, Just $ CT.utf16_be)
                0xFF : 0xFE: _           -> (2, Just $ CT.utf16_le)
                0xEF : 0xBB: 0xBF : _    -> (3, Just $ CT.utf8)
                [0x00, 0x00, 0x00, 0x3C] -> (0, Just $ CT.utf32_be)
                [0x3C, 0x00, 0x00, 0x00] -> (0, Just $ CT.utf32_le)
                [0x00, 0x3C, 0x00, 0x3F] -> (0, Just $ CT.utf16_be)
                [0x3C, 0x00, 0x3F, 0x00] -> (0, Just $ CT.utf16_le)
                _                        -> (0, Nothing) -- Assuming UTF-8

checkXMLDecl :: MonadThrow m
             => S.ByteString
             -> Maybe CT.Codec
             -> Conduit S.ByteString m TS.Text
checkXMLDecl bs (Just codec) = leftover bs >> CT.decode codec
checkXMLDecl bs0 Nothing =
    loop [] (AT.parse (parseToken decodeXmlEntities False)) bs0
  where
    loop chunks0 parser nextChunk =
        case parser $ decodeUtf8With lenientDecode nextChunk of
            AT.Fail _ _ _ -> fallback
            AT.Partial f -> await >>= maybe fallback (loop chunks f)
            AT.Done _ [TokenBeginDocument attrs] -> findEncoding attrs
            AT.Done _ _ -> fallback
      where
        chunks = nextChunk : chunks0
        fallback = complete CT.utf8
        complete codec = mapM_ leftover chunks >> CT.decode codec

        findEncoding [] = fallback
        findEncoding ((TName _ "encoding", [ContentText enc]):_) =
            case enc of
                "iso-8859-1" -> complete CT.iso8859_1
                "utf-8" -> complete CT.utf8
                _ -> complete CT.utf8
        findEncoding (_:xs) = findEncoding xs

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
--
-- Since 1.2.4
parseText' :: MonadThrow m
           => ParseSettings
           -> Conduit TS.Text m Event
parseText' = mapOutput snd . parseTextPos

{-# DEPRECATED parseText "Please use 'parseText'' or 'parseTextPos'." #-}
parseText :: MonadThrow m
          => ParseSettings
          -> Conduit TS.Text m EventPos
parseText = parseTextPos

-- | Same as 'parseText'', but includes the position of each event.
--
-- Since 1.2.4
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
    , psPreserveWhiteSpace :: Bool
    -- ^ Whether we should preserve literal XML whitespace within nodes.
    --
    -- Default: False
    }

instance Default ParseSettings where
    def = ParseSettings
        { psDecodeEntities = decodeXmlEntities
        , psRetainNamespaces = False
        , psPreserveWhiteSpace = False
        }

conduitToken :: MonadThrow m => ParseSettings -> Conduit TS.Text m (PositionRange, Token)
conduitToken = (=$= flattenWithPos) . conduitParser . uncurry parseToken . (psDecodeEntities &&& psPreserveWhiteSpace)

flattenWithPos :: MonadThrow m => Conduit (PositionRange, [Token]) m (PositionRange, Token)
flattenWithPos = await >>= maybe (return ()) go 
    where go (pos, xs) = do mapM_ (\x -> yield (pos, x)) xs
                            flattenWithPos

parseToken :: DecodeEntities -> Bool -> Parser [Token]
parseToken de preserveWS =
  nonContent <|> fmap return cdata <|> textNodes preserveWS <|> trailingWS
  where
    trailingWS = takeWhile1 isXMLSpace >> return []
    nonContent = do
        skipSpace
        void $ char '<'
        return <$> parseLt
    parseLt =
        (char '?' >> parseInstr) <|>
        (char '!' >> (parseComment <|> parseDoctype)) <|>
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
                x <- T.pack <$> manyTill anyChar (string "?>")
                return $ TokenInstruction $ Instruction name x
    parseComment = do
        void $ string "--"
        c <- T.pack <$> manyTill anyChar (string "-->") -- FIXME use takeWhile instead
        return $ TokenComment c 
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
    parseEntity = do
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
    textNode preserve = TokenContent <$> parseContent de preserve False False
    cdata = do
        void $ string "<![CDATA["
        t <- manyTill anyChar (string "]]>") -- FIXME use takeWhile instead
        return $ TokenCDATA (T.pack t)
    textNodes preserveWhiteSpace = do
        tn <- textNode preserveWhiteSpace
        tns <- AT.manyTill' (cdata <|> textNode True) noMoreTextNodes
        if preserveWhiteSpace
            then return (tn : tns)
            else return $ trimLastContent (tn : tns)
    noMoreTextNodes = do
        c <- AT.peekChar 
        case c of
            Nothing -> return ()
            Just '<' -> do continue <- nextIsCData
                           if continue then mzero else return ()
            _       -> mzero
        where nextIsCData = AC.option False
                              (AC.lookAhead (string "<![CDATA[") >> return True)

trimLastContent :: [Token] -> [Token]
trimLastContent [] = []
trimLastContent [TokenContent (ContentText t)] =
    case T.dropWhileEnd isXMLSpace t of
      "" -> []
      trimmed -> [TokenContent (ContentText trimmed)]
trimLastContent [t] = [t]
trimLastContent (t:ts) = t : trimLastContent ts

parseAttribute :: DecodeEntities -> Parser TAttribute
parseAttribute de = do
    skipSpace
    key <- parseName
    skipSpace
    char' '='
    skipSpace
    val <- (squoted <|> dquoted) <?> "attribute value"
    return (key, val)
  where
    squoted = char '\'' *> manyTill (parseContent de True False True) (char '\'')
    dquoted = char  '"' *> manyTill (parseContent de True True False) (char  '"')

-- The w3.org spec is clear about what defines a valid tag name:
--
--   NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
--                    | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF]
--                    | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF]
--                    | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
--                    | [#x10000-#xEFFFF]
--   NameChar      ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F]
--                    | [#x203F-#x2040]
--   Name          ::= NameStartChar (NameChar)*
--
-- Due to the semantic importance of ':', it is handled separately, but allowed to appear
-- in the local name.
--
-- We use range based tests to avoid testing each character.
parseName :: Parser TName
parseName = do
    c <- satisfy isStartChar <?> "NameStartChar"
    cs <- takeWhile nameChar <?> "NameChar"
    ln <- A.optional (char ':' >> localName <?> "LocalName")
    return $ name (T.cons c cs) ln
    where isStartChar = (inAnyRange validStartChars)
          nameChar = (inAnyRange validNameChars)
          localName = takeWhile1 (inAnyRange (only ':' : validNameChars))

          name i1 Nothing = TName Nothing i1
          name i1 (Just i2) = TName (Just i1) i2

          only c = (c, c)
          inAnyRange [] _ = False
          inAnyRange ((x, y):rs) c = if x <= c && c <= y then True else inAnyRange rs c
          validStartChars = [('A', 'Z'), ('a', 'z'), ('\xC0', '\xD6'), ('\xD8', '\xF6')
                            ,('\xF8', '\x2FF'),    ('\x370', '\x37D'),   ('\x37F', '\x1FFF')
                            ,('\x200C', '\x200D'), ('\x2070', '\x218F'), ('\x2C00', '\x2FEF')
                            ,('\x3001', '\xD7FF'), ('\xF900', '\xFDCF'), ('\xFDF0', '\xFFFD')
                            ,('\x10000', '\xEFFFF')]
          validNameChars = [only '-', only '.', only '\xB7', ('0', '9')] ++ validStartChars
                            ++ [('\x0300', '\x036F'), ('\x203F', '\x2040')]

parseIdent :: Parser Text
parseIdent =
    takeWhile1 valid <?> "valid identifier"
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
             -> Bool -- preserve XML whitespace
             -> Bool -- break on double quote
             -> Bool -- break on single quote
             -> Parser Content
parseContent de preserveWhiteSpace breakDouble breakSingle =
    parseEntity <|> parseText'
  where
    parseEntity = do
        char' '&'
        t <- takeWhile1 (/= ';') <?> "not a semicolon"
        char' ';'
        return $ de t
    parseText' = do
        leading <- if preserveWhiteSpace
                    then takeWhile isXMLSpace
                    else skipSpace >> return ""
        bs <- if not (T.null leading) && preserveWhiteSpace
                    then takeWhile valid <?> "valid content character"
                    else takeWhile1 valid <?> "at least one valid content character"
        le <- AT.option ""
              $ (string "\n" <|> string "\r\n" <|> string "\r") >> return "\n"
        return $ ContentText (leading <> bs <> le)
    valid '"' = not breakDouble
    valid '\'' = not breakSingle
    valid '&' = False -- amp
    valid '<' = False -- lt
    valid '\r' = False -- lt
    valid '\n' = False -- lt
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
isXMLSpace '\x20' = True
isXMLSpace '\x09' = True
isXMLSpace '\x0D' = True
isXMLSpace '\x0A' = True
isXMLSpace _ = False

-- extends Attoparsec.newLine to also accept single '\r'
newline :: Parser ()
newline = endOfLine <|> (void $ char '\r')

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
    x <- scanPastSkippable
    case x of
        Just (EventBeginElement name as) ->
            case checkName name of
                Just y ->
                    case runAttrParser' (attrParser y) as of
                        Left e -> lift $ monadThrow e
                        Right z -> do
                            CL.drop 1
                            z' <- f z
                            a <- scanPastSkippable
                            case a of
                                Just (EventEndElement name')
                                    | name == name' -> CL.drop 1 >> return (Just z')
                                _ -> lift $ monadThrow $ XmlException ("Expected end tag for: " ++ show name) a
                Nothing -> return Nothing
        _ -> return Nothing
  where
    -- Drop Events comments and other meta-events
    scanPastSkippable = do
        x <- CL.peek
        let skippable =
                case x of
                    Just EventComment{} -> True
                    Just EventBeginDocument -> True
                    -- Just EventEndDocument -> True
                    Just EventBeginDoctype{} -> True
                    Just EventEndDoctype -> True
                    Just EventInstruction{} -> True
                    _ -> False
        if skippable then CL.drop 1 >> scanPastSkippable else return x
    runAttrParser' p as =
        case runAttrParser p as of
            Left e -> Left e
            Right ([], x) -> Right x
            Right (attr, _) -> Left $ toException $ UnparsedAttributes attr

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


-- | A further simplified tag parser, which ignores all attributes, if any exist
tagIgnoreAttrs :: MonadThrow m
               => Name -- ^ The name this parser matches to
               -> CI.ConduitM Event o m a -- ^ Handler function to handle the children of the matched tag
               -> CI.ConduitM Event o m (Maybe a)
tagIgnoreAttrs name f = tagName name ignoreAttrs $ const f

-- | A further simplified tag parser, which ignores all attributes, if any exist
tagPredicateIgnoreAttrs :: MonadThrow m
                        => (Name -> Bool) -- ^ The name predicate this parser matches to
                        -> CI.ConduitM Event o m a -- ^ Handler function to handle the children of the matched tag
                        -> CI.ConduitM Event o m (Maybe a)
tagPredicateIgnoreAttrs namePred f = tagPredicate namePred ignoreAttrs $ const f

-- | Ignore an empty tag and all of its attributes by predicate.
--   This does not ignore the tag recursively
--   (i.e. it assumes there are no child elements).
--   This functions returns 'Just' if the tag matched.
ignoreTag :: MonadThrow m
          => (Name -> Bool) -- ^ The predicate name to match to
          -> ConduitM Event o m (Maybe ())
ignoreTag namePred = tagPredicateIgnoreAttrs namePred (return ())

-- | Like 'ignoreTag', but matches an exact name
ignoreTagName :: MonadThrow m
              => Name -- ^ The name to match to
              -> ConduitM Event o m (Maybe ())
ignoreTagName name = ignoreTag (== name)

-- | Like 'ignoreTagName', but matches any name from a list of names.
ignoreAnyTagName :: MonadThrow m
                 => [Name] -- ^ The name to match to
                 -> ConduitM Event o m (Maybe ())
ignoreAnyTagName names = ignoreTag (`elem` names)

-- | Like 'ignoreTag', but matches all tag name.
--
--   > ignoreAllTags = ignoreTag (const True)
ignoreAllTags :: MonadThrow m => ConduitM Event o m (Maybe ())
ignoreAllTags = ignoreTag $ const True

-- | Ignore an empty tag, its attributes and its children subtree recursively.
--   Both content and text events are ignored.
--   This functions returns 'Just' if the tag matched.
ignoreTree :: MonadThrow m
          => (Name -> Bool) -- ^ The predicate name to match to
          -> ConduitM Event o m (Maybe ())
ignoreTree namePred =
    tagPredicateIgnoreAttrs namePred (const () <$> many ignoreAllTreesContent)

-- | Like 'ignoreTagName', but also ignores non-empty tabs
ignoreTreeName :: MonadThrow m
               => Name
               -> ConduitM Event o m (Maybe ())
ignoreTreeName name = ignoreTree (== name)

-- | Like 'ignoreTagName', but matches any name from a list of names.
ignoreAnyTreeName :: MonadThrow m
                 => [Name] -- ^ The name to match to
                 -> ConduitM Event o m (Maybe ())
ignoreAnyTreeName names = ignoreTree (`elem` names)

-- | Like 'ignoreAllTags', but ignores entire subtrees.
--
--   > ignoreAllTrees = ignoreTree (const True)
ignoreAllTrees :: MonadThrow m => ConduitM Event o m (Maybe ())
ignoreAllTrees = ignoreTree $ const True

-- | Like 'ignoreAllTrees', but also ignores all content events
ignoreAllTreesContent :: MonadThrow m => ConduitM Event o m (Maybe ())
ignoreAllTreesContent = (void <$> contentMaybe) `orE` ignoreAllTrees

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
       => [ConduitM Event o m (Maybe a)] -- ^ List of parsers that will be tried in order.
       -> ConduitM Event o m (Maybe a)   -- ^ Result of the first parser to succeed, or @Nothing@
                                         --   if no parser succeeded
choose [] = return Nothing
choose (i:is) =
    i >>= maybe (choose is) (return . Just)

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
          -> Producer m Event
parseFile ps fp = sourceFile fp =$= parseBytes ps

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
    maybe (AttrParser $ const $ Left $ toException $ XmlException msg Nothing)
          return

-- | Return the value for an attribute if present.
attr :: Name -> AttrParser (Maybe Text)
attr = optionalAttr

-- | Shortcut composition of 'force' and 'attr'.
requireAttr :: Name -> AttrParser Text
requireAttr n = force ("Missing attribute: " ++ show n) $ attr n

{-# DEPRECATED optionalAttr "Please use 'attr'." #-}
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

-- | Keep parsing elements as long as the parser returns 'Just'
--   or the ignore parser returns 'Just'.
manyIgnore :: Monad m
           => Consumer Event m (Maybe a)
           -> Consumer Event m (Maybe ())
           -> Consumer Event m [a]
manyIgnore i ignored =
    go id
  where
    go front = i >>=
        maybe (onFail front) (\y -> go $ front . (:) y)
    -- onFail is called if the main parser fails
    onFail front =
        ignored >>= maybe (return $ front []) (const $ go front)

-- | Like @many@, but any tags and content the consumer doesn't match on
--   are silently ignored.
many' :: MonadThrow m
           => Consumer Event m (Maybe a)
           -> Consumer Event m [a]
many' consumer = manyIgnore consumer ignoreAllTreesContent

-- | Like 'many', but uses 'yield' so the result list can be streamed
--   to downstream conduits without waiting for 'manyYield' to finished
manyYield :: Monad m
          => ConduitM a b m (Maybe b)
          -> Conduit a m b
manyYield consumer =
    loop
  where
    loop = consumer >>= maybe (return ()) (\x -> yield x >> loop)

-- | Like @manyIgnore@, but uses 'yield' so the result list can be streamed
--   to downstream conduits without waiting for 'manyYield' to finished
manyIgnoreYield :: MonadThrow m
                => ConduitM Event b m (Maybe b) -- ^ Consuming parser that generates the result stream
                -> Consumer Event m (Maybe ()) -- ^ Ignore parser that consumes elements to be ignored
                -> Conduit Event m b
manyIgnoreYield consumer ignoreParser =
    loop
  where
    loop = consumer >>= maybe onFail (\x -> yield x >> loop)
    onFail = ignoreParser >>= maybe (return ()) (const loop)

-- | Like @many'@, but uses 'yield' so the result list can be streamed
--   to downstream conduits without waiting for 'manyYield' to finished
manyYield' :: MonadThrow m
           => ConduitM Event b m (Maybe b)
           -> Conduit Event m b
manyYield' consumer = manyIgnoreYield consumer ignoreAllTreesContent

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
