{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module provides both a native Haskell solution for parsing XML
-- documents into a stream of events, and a set of parser combinators for
-- dealing with a stream of events.
--
-- The important thing to know about the combinators is that they do /not/ work
-- on the fully-powered 'Event' datatype; rather, this module defines an
-- 'SEvent' datatype which only deals with tags, attributes and content. For
-- most uses, this is sufficient. If you need to parse doctypes, instructions
-- or contents, you will not be able to use the combinators.
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
-- > import Text.XML.Enumerator.Parse
-- > import Data.Text.Lazy (Text, unpack)
-- > 
-- > data Person = Person { age :: Int, name :: Text }
-- >     deriving Show
-- > 
-- > parsePerson = tag' "person" (requireAttr "age") $ \age -> do
-- >     name <- content'
-- >     return $ Person (read $ unpack age) name
-- > 
-- > parsePeople = tag'' "people" $ many parsePerson
-- > 
-- > main = parseFile_ "people.xml" (const Nothing) $ force "people required" parsePeople
--
-- will produce:
--
-- > [Person {age = 25, name = "Michael"},Person {age = 2, name = "Eliezer"}]
module Text.XML.Enumerator.Parse
    ( -- * Parsing XML files
      parseBytes
    , parseText
    , detectUtf
      -- * Simplified events
    , SEvent (..)
    , simplify
    , SAttr
    , parseFile
    , parseFile_
      -- * SEvent parsing
    , tag
    , tag'
    , tagName
    , tag''
    , tagNoAttr
    , content
    , content'
      -- * Attribute parsing
    , AttrParser
    , requireAttr
    , optionalAttr
    , requireAttrRaw
    , optionalAttrRaw
    , ignoreAttrs
      -- * Combinators
    , choose
    , many
    , force
      -- * Exceptions
    , XmlException (..)
    ) where
import Data.Attoparsec.Text
    ( char, Parser, takeWhile1, skipWhile, string
    , manyTill, takeWhile, try, anyChar, endOfInput, hexadecimal, decimal
    )
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text.Enumerator (iterParser)
import Data.XML.Types
    ( Name (..), Event (..), Content (..), Attribute (..)
    , Doctype (..), Instruction (..), ExternalID (..)
    )
import Control.Applicative ((<|>), (<$>))
import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import Text.XML.Enumerator.Token
import Prelude hiding (takeWhile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.Enumerator (Iteratee, Enumeratee, (>>==), Stream (..),
                        checkDone, yield, ($$), joinI, run, throwError)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.Binary as EB
import Control.Monad (unless, ap, liftM)
import qualified Data.Text as TS
import Data.List (foldl')
import Control.Applicative (Applicative (..))
import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO, SomeException)
import Data.Enumerator.Binary (enumFile)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)

tokenToEvent :: [NSLevel] -> Token -> ([NSLevel], [Event])
tokenToEvent n (TokenBeginDocument _) = (n, [])
tokenToEvent n (TokenInstruction i) = (n, [EventInstruction i])
tokenToEvent n (TokenBeginElement name as isClosed) =
    (n', if isClosed then [begin, end] else [begin])
  where
    l0 = case n of
            [] -> NSLevel Nothing Map.empty
            x:_ -> x
    (as', l') = foldl' go (id, l0) as
    go (front, l) a@(TName kpref kname, val)
        | kpref == Just "xmlns" =
            (front, l { prefixes = Map.insert kname (contentsToText val)
                                 $ prefixes l })
        | kpref == Nothing && kname == "xmlns" =
            (front, l { defaultNS = if T.null $ contentsToText val
                                        then Nothing
                                        else Just $ contentsToText val })
        | otherwise = (front . (:) a, l)
    n' = if isClosed then n else l' : n
    contentsToText = T.concat . map helper
    helper (ContentText t) = t
    helper (ContentEntity _) = T.empty -- FIXME
    fixAttName level (name', val) = Attribute (tnameToName True level name') val
    begin = EventBeginElement (tnameToName False l' name) $ map (fixAttName l') $ as' []
    end = EventEndElement $ tnameToName False l' name
tokenToEvent n (TokenEndElement name) =
    (n', [EventEndElement $ tnameToName False l name])
  where
    (l, n') =
        case n of
            [] -> (NSLevel Nothing Map.empty, [])
            x:xs -> (x, xs)
tokenToEvent n (TokenContent c) = (n, [EventContent c])
tokenToEvent n (TokenComment c) = (n, [EventComment c])
tokenToEvent n (TokenDoctype t eid) = (n, [EventDoctype $ Doctype t eid []])

tnameToName :: Bool -> NSLevel -> TName -> Name
tnameToName _ _ (TName (Just "xml") name) =
    Name name (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")
tnameToName isAttr (NSLevel def _) (TName Nothing name) =
    Name name (if isAttr then Nothing else def) Nothing
tnameToName _ (NSLevel _ m) (TName (Just pref) name) =
    case Map.lookup pref m of
        Just ns -> Name name (Just ns) (Just pref)
        Nothing -> Name name Nothing (Just pref) -- FIXME is this correct?

-- | Automatically determine which UTF variant is being used. This function
-- first checks for BOMs, removing them as necessary, and then check for the
-- equivalent of <?xml for each of UTF-8, UTF-16LE/BE, and UTF-32LE/BE. It
-- defaults to assuming UTF-8.
detectUtf :: Monad m => Enumeratee S.ByteString TS.Text m a
detectUtf step = do
    x <- EB.take 4
    let (toDrop, codec) =
            case L.unpack x of
                [0x00, 0x00, 0xFE, 0xFF] -> (4, ET.utf32_be)
                [0xFF, 0xFE, 0x00, 0x00] -> (4, ET.utf32_le)
                0xFE : 0xFF: _           -> (2, ET.utf16_be)
                0xFF : 0xFE: _           -> (2, ET.utf16_le)
                0xEF : 0xBB: 0xBF : _    -> (3, ET.utf8)
                [0x00, 0x00, 0x00, 0x3C] -> (0, ET.utf32_be)
                [0x3C, 0x00, 0x00, 0x00] -> (0, ET.utf32_le)
                [0x00, 0x3C, 0x00, 0x3F] -> (0, ET.utf16_be)
                [0x3C, 0x00, 0x3F, 0x00] -> (0, ET.utf16_le)
                _                        -> (0, ET.utf8) -- Assuming UTF-8
    unless (toDrop == 4) $ yield () $ Chunks $ L.toChunks $ L.drop toDrop x
    ET.decode codec step

-- | Parses a byte stream into 'Event's. This function is implemented fully in
-- Haskell using attoparsec-text for parsing. The produced error messages do
-- not give line/column information, so you may prefer to stick with the parser
-- provided by libxml-enumerator. However, this has the advantage of not
-- relying on any C libraries.
--
-- This relies on 'detectUtf' to determine character encoding, and 'parseText'
-- to do the actual parsing.
parseBytes :: Monad m => Enumeratee S.ByteString Event m a
parseBytes step = joinI $ detectUtf $$ parseText step

-- | Parses a character stream into 'Event's. This function is implemented
-- fully in Haskell using attoparsec-text for parsing. The produced error
-- messages do not give line/column information, so you may prefer to stick
-- with the parser provided by libxml-enumerator. However, this has the
-- advantage of not relying on any C libraries.
parseText :: Monad m => Enumeratee TS.Text Event m a
parseText =
    checkDone $ \k -> k (Chunks [EventBeginDocument]) >>== loop []
  where
    loop levels = checkDone $ go levels
    go levels k = do
        mtoken <- iterToken
        case mtoken of
            Nothing -> k (Chunks [EventEndDocument]) >>== return
            Just token ->
                let (levels', events) = tokenToEvent levels token
                 in k (Chunks events) >>== loop levels'

iterToken :: Monad m => Iteratee TS.Text m (Maybe Token)
iterToken = iterParser ((endOfInput >> return Nothing) <|> fmap Just parseToken)

parseToken :: Parser Token
parseToken = do
    (char '<' >> parseLt) <|> fmap TokenContent (parseContent False False)
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
                as <- A.many parseAttribute
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
        return $ TokenContent $ ContentText t
    parseDoctype = do
        _ <- string "DOCTYPE"
        skipSpace
        i <- parseIdent
        skipSpace
        eid <- fmap Just parsePublicID <|>
               fmap Just parseSystemID <|>
               return Nothing
        skipSpace
        (do
            char' '['
            skipWhile (/= ']')
            char' ']'
            skipSpace) <|> return ()
        char' '>'
        newline <|> return ()
        return $ TokenDoctype i eid
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
        T.fromChunks . return <$> (between '"' <|> between '\'')
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
        as <- A.many parseAttribute
        skipSpace
        isClose <- (char '/' >> skipSpace >> return True) <|> return False
        char' '>'
        return $ TokenBeginElement n as isClose

parseAttribute :: Parser TAttribute
parseAttribute = do
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
        manyTill (parseContent False True) (char '\'')
    dquoted = do
        char' '"'
        manyTill (parseContent True False) (char '"')

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
    T.fromChunks . return <$> takeWhile1 valid
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

parseContent :: Bool -- break on double quote
             -> Bool -- break on single quote
             -> Parser Content
parseContent breakDouble breakSingle =
    parseEntity <|> parseText'
  where
    parseEntity = do
        char' '&'
        parseEntityNum <|> parseEntityWord
    parseEntityNum = do
        char' '#'
        w <- parseEntityHex <|> parseEntityDig
        return $ ContentText $ pack [toEnum w]
    parseEntityHex = do
        char' 'x'
        res <- hexadecimal
        char' ';'
        return res
    parseEntityDig = do
        res <- decimal
        char' ';'
        return res
    parseEntityWord = do
        s <- takeWhile1 (/= ';')
        char' ';'
        return $ case s of
            _
                | s == "amp"  -> ContentText "&"
                | s == "gt"   -> ContentText ">"
                | s == "lt"   -> ContentText "<"
                | s == "apos" -> ContentText "'"
                | s == "quot" -> ContentText "\""
                | otherwise   ->
                    ContentEntity $ T.fromChunks [s]
    parseText' = do
        bs <- takeWhile1 valid
        return $ ContentText $ T.fromChunks [bs]
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

-- | A simplified attribute, having all entities converted to text.
type SAttr = (Name, Text)

-- | A greatly simplified XML event datatype. The best way to produce these
-- values is the 'simplify' enumeratee.
data SEvent = SBeginElement Name [SAttr]
            | SEndElement
            | SContent Text
    deriving (Show, Eq)

-- | Grabs the next piece of content if available.
content :: Monad m => Iteratee SEvent m (Maybe Text)
content = do
    x <- E.peek
    case x of
        Just (SContent t) -> EL.drop 1 >> return (Just t)
        _ -> return Nothing

-- | Grabs the next piece of content. If none if available, returns 'T.empty'.
content' :: Monad m => Iteratee SEvent m Text
content' = do
    x <- content
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
tag :: Monad m
    => (Name -> Maybe a)
    -> (a -> AttrParser b)
    -> (b -> Iteratee SEvent m c)
    -> Iteratee SEvent m (Maybe c)
tag checkName attrParser f = do
    x <- dropWS
    case x of
        Just (SBeginElement name as) ->
            case checkName name of
                Just y ->
                    case runAttrParser' (attrParser y) as of
                        Left e -> throwError e
                        Right z -> do
                            EL.drop 1
                            z' <- f z
                            a <- dropWS
                            case a of
                                Just SEndElement -> EL.drop 1 >> return (Just z')
                                _ -> throwError $ SXmlException ("Expected end tag for: " ++ show name) a
                Nothing -> return Nothing
        _ -> return Nothing
  where
    dropWS = do
        x <- E.peek
        case x of
            Just (SContent t)
                | T.all isSpace t -> EL.drop 1 >> E.peek
            _ -> return x
    runAttrParser' p as =
        case runAttrParser p as of
            Left e -> Left e
            Right ([], x) -> Right x
            Right (attr, _) -> Left $ UnparsedAttributes attr

-- | A simplified version of 'tag' which matches for specific tag names instead
-- of taking a predicate function. This is often sufficient, and when combined
-- with OverloadedStrings and the IsString instance of 'Name', can prove to be
-- very concise.
tag', tagName :: Monad m
     => Name
     -> AttrParser a
     -> (a -> Iteratee SEvent m b)
     -> Iteratee SEvent m (Maybe b)
tag' name attrParser = tag
    (\x -> if x == name then Just () else Nothing)
    (const attrParser)
tagName = tag'

{-# DEPRECATED tag' "Use tagName instead" #-}

-- | A further simplified tag parser, which requires that no attributes exist.
tag'', tagNoAttr :: Monad m => Name -> Iteratee SEvent m a -> Iteratee SEvent m (Maybe a)
tag'' name f = tag' name (return ()) $ const f
tagNoAttr = tag''

{-# DEPRECATED tag'' "Use tagNoAttr instead" #-}

-- | Get the value of the first parser which returns 'Just'. If none return
-- 'Just', returns 'Nothing'.
choose :: Monad m
       => [Iteratee SEvent m (Maybe a)]
       -> Iteratee SEvent m (Maybe a)
choose [] = return Nothing
choose (i:is) = do
    x <- i
    case x of
        Nothing -> choose is
        Just a -> return $ Just a

-- | Force an optional parser into a required parser. All of the 'tag'
-- functions, 'choose' and 'many' deal with 'Maybe' parsers. Use this when you
-- want to finally force something to happen.
force :: Monad m
      => String -- ^ Error message
      -> Iteratee SEvent m (Maybe a)
      -> Iteratee SEvent m a
force msg i = do
    x <- i
    case x of
        Nothing -> throwError $ XmlException msg Nothing
        Just a -> return a

-- | Convert a stream of 'Event's into a stream 'SEvent's. The first argument
-- is a function to decode character entity references. Some things to note
-- about this function:
--
-- * It drops events for document begin/end, comments, and instructions.
--
-- * It concatenates all pieces of content together. The output of this
-- function is guaranteed to not have two consecutive 'SContent's.
--
-- * It automatically checks that tag beginnings and endings are well balanced,
-- and throws an exception otherwise.
--
-- * It also throws an exception if your supplied entity function does not know
-- how to deal with a character entity.
--
-- Please also note that you do /not/ need to handle the 5 XML-defined
-- character entity references (lt, gt, amp, quot and apos), nor deal with
-- numeric entities (decimal and hex).
simplify :: Monad m => (Text -> Maybe Text) -> Enumeratee Event SEvent m b
simplify renderEntity =
    loop []
  where
    loop stack = E.checkDone $ go stack
    sattr (Attribute x y) = do
        y' <- flip mapM y $ \z ->
            case z of
                ContentText t -> return t
                ContentEntity t ->
                    case renderEntity t of
                        Just t' -> return t'
                        Nothing -> throwError $ InvalidEntity t
        return (x, T.concat y')
    go stack k = do
        x <- EL.head
        case x of
            Nothing -> k EOF >>== return
            Just EventBeginDocument -> go stack k
            Just EventEndDocument ->
                k EOF >>== return
            Just EventInstruction{} -> go stack k
            Just EventDoctype{} -> go stack k
            Just (EventBeginElement n as) -> do
                as' <- mapM sattr as
                k (Chunks [SBeginElement n as']) >>== loop (n : stack)
            Just (EventEndElement n) ->
                case stack of
                    [] -> throwError $ InvalidEndElement n
                    n':rest
                        | n == n' -> k (Chunks [SEndElement]) >>== loop rest
                        | otherwise -> throwError $ InvalidEndElement n
            Just (EventContent c) -> do
                t <- contentToText c
                ts <- takeContents $ (:) t
                k (Chunks [SContent $ T.concat $ ts []]) >>== loop stack
            Just EventComment{} -> go stack k
      where
        contentToText (ContentEntity e) =
            case renderEntity e of
                Nothing -> throwError $ InvalidEntity e
                Just t -> return t
        contentToText (ContentText t) = return t
        takeContents front = do
            x <- E.peek
            case x of
                Nothing -> return front
                Just EventBeginElement{} -> return front
                Just EventEndElement{} -> return front
                Just (EventContent c) -> do
                    EL.drop 1
                    t <- contentToText c
                    takeContents $ front . (:) t
                Just EventBeginDocument -> helper
                Just EventEndDocument -> helper
                Just EventInstruction{} -> helper
                Just EventDoctype{} -> helper
                Just EventComment{} -> helper
          where
            helper = EL.drop 1 >> takeContents front

-- | The same as 'parseFile', but throws any exceptions.
parseFile_ :: String -> (Text -> Maybe Text) -> Iteratee SEvent IO a -> IO a
parseFile_ fn re p =
    parseFile fn re p >>= go
  where
    go (Left e) = liftIO $ throwIO e
    go (Right a) = return a

-- | A helper function which reads a file from disk using 'enumFile', detects
-- character encoding using 'detectUtf', parses the XML using 'parseBytes',
-- converts to an 'SEvent' stream using 'simplify' and then handing off control
-- to your supplied parser.
parseFile :: String -> (Text -> Maybe Text) -> Iteratee SEvent IO a -> IO (Either SomeException a)
parseFile fn re p =
    run $ enumFile fn     $$ joinI
        $ parseBytes      $$ joinI
        $ simplify re     $$ p

data XmlException = XmlException
    { xmlErrorMessage :: String
    , xmlBadInput :: Maybe Event
    }
                  | InvalidEndElement Name
                  | InvalidEntity Text
                  | SXmlException
    { xmlErrorMessage :: String
    , sxmlBadInput :: Maybe SEvent
    }
                  | UnparsedAttributes [SAttr]
    deriving (Show, Typeable)
instance Exception XmlException

-- | A monad for parsing attributes. By default, it requires you to deal with
-- all attributes present on an element, and will throw an exception if there
-- are unhandled attributes. Use the 'requireAttr', 'optionalAttr' et al
-- functions for handling an attribute, and 'ignoreAttrs' if you would like to
-- skip the rest of the attributes on an element.
newtype AttrParser a = AttrParser { runAttrParser :: [SAttr] -> Either XmlException ([SAttr], a) }

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

optionalAttrRaw :: (SAttr -> Maybe b) -> AttrParser (Maybe b)
optionalAttrRaw f =
    AttrParser $ go id
  where
    go front [] = Right (front [], Nothing)
    go front (a:as) =
        case f a of
            Nothing -> go (front . (:) a) as
            Just b -> Right (front as, Just b)

requireAttrRaw :: String -> (SAttr -> Maybe b) -> AttrParser b
requireAttrRaw msg f = do
    x <- optionalAttrRaw f
    case x of
        Just b -> return b
        Nothing -> AttrParser $ const $ Left $ XmlException msg Nothing

-- | Require that a certain attribute be present and return its value.
requireAttr :: Name -> AttrParser Text
requireAttr n = requireAttrRaw
    ("Missing attribute: " ++ show n)
    (\(x, y) -> if x == n then Just y else Nothing)

-- | Return the value for an attribute if present.
optionalAttr :: Name -> AttrParser (Maybe Text)
optionalAttr n = optionalAttrRaw
    (\(x, y) -> if x == n then Just y else Nothing)

-- | Skip the remaining attributes on an element. Since this will clear the
-- list of attributes, you must call this /after/ any calls to 'requireAttr',
-- 'optionalAttr', etc.
ignoreAttrs :: AttrParser ()
ignoreAttrs = AttrParser $ \_ -> Right ([], ())

-- | Keep parsing elements as long as the parser returns 'Just'.
many :: Monad m => Iteratee SEvent m (Maybe a) -> Iteratee SEvent m [a]
many i =
    go id
  where
    go front = do
        x <- i
        case x of
            Nothing -> return $ front []
            Just y -> go $ front . (:) y

{-
-- There is some possible realisations using higher interface
-- ignoreSiblings' is about 30 percent slowly than ignoreSiblings
-- if ignoreSiblings' uses ignoreElem (instead of ignoreElem') it is about 5 percent slowly than ignoreSiblings 

-- | Ignore  content if exists
ignoreContent :: Monad m => Iteratee SEvent m (Maybe ())
ignoreContent = fmap (fmap $ const ()) content
-- | Iteratee to skip the next element. 
ignoreElem' :: Monad m => Iteratee SEvent m (Maybe ())
ignoreElem' = tag (const $ Just ()) (const ignoreAttrs) (const $ ignoreSiblings' >> return ())

-- | Iteratee to skip the siblings element. 
ignoreSiblings' :: Monad m => Iteratee SEvent m [()]
ignoreSiblings' = many (choose [ignoreElem', ignoreContent])
-}

-- | Iteratee to skip the siblings element. 
ignoreSiblings :: Monad m => Iteratee SEvent m ()
ignoreSiblings = E.continue (loop 0) 
  where
    loop :: Monad m => Int -> Stream SEvent -> Iteratee SEvent m ()
    loop n (Chunks []) = E.continue (loop n)
    loop n chs@(Chunks (x:_)) = case x of
        (SBeginElement _ _) -> E.continue (loop (n+1))
        SEndElement 
            | n == 0    -> yield () chs 
            | otherwise -> E.continue (loop (n-1))
        _ -> E.continue (loop n)
    loop _ EOF = throwError $ XmlException "Unbalanced xml-tree. (Error in skipSiblings)" Nothing

-- | Iteratee to skip the next element. 
ignoreElem :: Monad m => Iteratee SEvent m (Maybe ())
ignoreElem = E.continue (loop 0) 
  where
    loop :: Monad m => Int -> Stream SEvent -> Iteratee SEvent m (Maybe ())
    loop n (Chunks []) = E.continue (loop n)
    loop n chs@(Chunks (x:xs)) = case x of
        (SBeginElement _ _) -> E.continue (loop (n+1))
        SEndElement 
            | n == 0    -> yield Nothing chs 
            | n == 1    -> yield (Just ()) (Chunks xs) 
            | otherwise -> E.continue (loop (n-1))
        _ -> E.continue (loop n)
    loop _ EOF = throwError $ XmlException "Unbalanced xml-tree. (Error in skipSiblings)" Nothing
    
-- | Skip the siblings elements until iteratee not right. 
skipTill :: Monad m => Iteratee SEvent m (Maybe a) -> Iteratee SEvent m (Maybe a)
skipTill i = go
  where
    go = i >>= \x -> case x of
        Nothing -> ignoreElem >>= (\x -> if x == Nothing then return Nothing else go)
        r -> return r

-- | Combinator to skip the siblings element. 
skipSiblings :: Monad m => Iteratee SEvent m a -> Iteratee SEvent m a
skipSiblings i = i >>= \r -> ignoreSiblings >> return r
