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
    , tag''
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
import Data.Attoparsec hiding (many)
import qualified Data.Attoparsec as A
import Data.Attoparsec.Enumerator
import Data.XML.Types
import Data.Word (Word8)
import Control.Applicative ((<|>), (<$>))
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Lazy (fromChunks, pack, Text)
import qualified Data.Text.Lazy as T
import Data.Text.Encoding.Error (lenientDecode)
import Text.XML.Enumerator.Token
import Prelude hiding (takeWhile)
import qualified Data.ByteString as S
import qualified Data.Map as Map
import Data.Enumerator (Iteratee, Enumeratee, (>>==), Stream (..),
                        checkDone, yield, ($$), joinI, run, throwError)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import Control.Monad (unless, ap, liftM)
import qualified Data.Text as TS
import Data.List (foldl')
import Control.Applicative (Applicative (..))
import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO, SomeException)
import Data.Enumerator.Binary (enumFile)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)

amp, hash, charx, semicolon, char0, char9, charA, charZ, chara, charz
   , colon, equal, squote, dquote, lt, gt, qmark, fslash, exmark, dash
   , lsquare, rsquare
    :: Word8
amp = 38
hash = 35
charx = 120
semicolon = 59
char0 = 48
char9 = 57
charA = 65
charZ = 90
chara = 97
charz = 122
colon = 58
equal = 61
squote = 39
dquote = 34
lt = 60
gt = 62
qmark = 63
fslash = 47
exmark = 33
dash = 45
lsquare = 91
rsquare = 93

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
-- defaults to assuming UTF-8. The output byte stream is guaranteed to be valid
-- UTF-8 bytes.
detectUtf :: Monad m => Enumeratee S.ByteString S.ByteString m a
detectUtf param = do
    x <- takeFourBytes S.empty
    let (toDrop, mcodec) =
            case S.unpack x of
                [0x00, 0x00, 0xFE, 0xFF] -> (4, Just ET.utf32_be)
                [0xFF, 0xFE, 0x00, 0x00] -> (4, Just ET.utf32_le)
                0xFE : 0xFF: _           -> (2, Just ET.utf16_be)
                0xFF : 0xFE: _           -> (2, Just ET.utf16_le)
                0xEF : 0xBB: 0xBF : _    -> (3, Nothing)
                [0x00, 0x00, 0x00, 0x3C] -> (0, Just ET.utf32_be)
                [0x3C, 0x00, 0x00, 0x00] -> (0, Just ET.utf32_le)
                [0x00, 0x3C, 0x00, 0x3F] -> (0, Just ET.utf16_be)
                [0x3C, 0x00, 0x3F, 0x00] -> (0, Just ET.utf16_le)
                _                        -> (0, Nothing) -- Assuming UTF-8
    unless (toDrop == 4) $ yield () $ Chunks [S.drop toDrop x]
    iter <-
      case mcodec of
        Nothing -> return param
        Just codec -> (joinI $ ET.decode codec $$ joinI $ ET.encode ET.utf8 param) >>== return
    E.map id iter
  where
    takeFourBytes front = do
        x <- EL.head
        case x of
            Nothing -> return front
            Just y -> do
                let z = S.append front y
                if S.length z < 4
                    then takeFourBytes z
                    else do
                        let (a, b) = S.splitAt 4 z
                        E.yield a $ Chunks [b]

-- | Parses a UTF8-encoded byte stream into 'Event's. This function is
-- implemented fully in Haskell using attoparsec for parsing. The produced
-- error messages do not give line/column information, so you may prefer to
-- stick with the parser provided by libxml-enumerator. However, this has the
-- advantage of not relying on any C libraries.
--
-- If you are uncertain of the character encoding, use the 'detectUtf'
-- enumeratee.
parseBytes :: Monad m => Enumeratee S.ByteString Event m a
parseBytes =
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

iterToken :: Monad m => Iteratee S.ByteString m (Maybe Token)
iterToken = iterParser ((endOfInput >> return Nothing) <|> fmap Just parseToken)

parseToken :: Parser Token
parseToken = do
    (word8 lt >> parseLt) <|> fmap TokenContent (parseContent False False)
  where
    parseLt =
        (word8 qmark >> parseInstr) <|>
        (word8 exmark >> (parseComment <|> parseCdata <|> parseDoctype)) <|>
        (word8 fslash >> parseEnd) <|>
        parseBegin
    parseInstr = do
        name <- parseIdent
        if name == "xml"
            then do
                as <- A.many parseAttribute
                skipSpace
                word8' qmark
                word8' gt
                newline <|> return ()
                return $ TokenBeginDocument as
            else do
                skipSpace
                x <- toText . S.pack <$> manyTill anyWord8 (try $ string "?>")
                return $ TokenInstruction $ Instruction name x
    parseComment = do
        word8' dash
        word8' dash
        c <- toText . S.pack <$> manyTill anyWord8 (string "-->") -- FIXME use takeWhile instead
        return $ TokenComment c
    parseCdata = do
        _ <- string "[CDATA["
        t <- toText . S.pack <$> manyTill anyWord8 (string "]]>") -- FIXME use takeWhile instead
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
            word8' lsquare
            skipWhile (/= rsquare)
            word8' rsquare
            skipSpace) <|> return ()
        word8' gt
        newline
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
        toText <$> (between dquote <|> between squote)
    between c = do
        word8' c
        x <- takeWhile (/=c)
        word8' c
        return x
    parseEnd = do
        skipSpace
        n <- parseName
        skipSpace
        word8' gt
        return $ TokenEndElement n
    parseBegin = do
        skipSpace
        n <- parseName
        as <- A.many parseAttribute
        skipSpace
        isClose <- (word8 fslash >> skipSpace >> return True) <|> return False
        word8' gt
        return $ TokenBeginElement n as isClose

parseAttribute :: Parser TAttribute
parseAttribute = do
    skipSpace
    key <- parseName
    skipSpace
    word8' equal
    skipSpace
    val <- squoted <|> dquoted
    return (key, val)
  where
    squoted = do
        word8' squote
        manyTill (parseContent False True) (word8 squote)
    dquoted = do
        word8' dquote
        manyTill (parseContent True False) (word8 dquote)

parseName :: Parser TName
parseName = do
    i1 <- parseIdent
    mi2 <- (word8 colon >> fmap Just parseIdent) <|> return Nothing
    return $
        case mi2 of
            Nothing -> TName Nothing i1
            Just i2 -> TName (Just i1) i2

parseIdent :: Parser Text
parseIdent =
    toText <$> takeWhile1 valid
  where
    valid 38 = False -- amp
    valid 60 = False -- lt
    valid 62 = False -- gt
    valid 58 = False -- colon
    valid 63 = False -- qmark
    valid 61 = False -- equal
    valid 34 = False -- dquote
    valid 39 = False -- squote
    valid 47 = False -- fslash
    valid c  = not $ isSpaceW c

isSpaceW :: Word8 -> Bool
isSpaceW 0x09 = True
isSpaceW 0x0A = True
isSpaceW 0x0D = True
isSpaceW 0x20 = True
isSpaceW _    = False

parseContent :: Bool -- break on double quote
             -> Bool -- break on single quote
             -> Parser Content
parseContent breakDouble breakSingle =
    parseEntity <|> parseText
  where
    parseEntity = do
        word8' amp
        parseEntityNum <|> parseEntityWord
    parseEntityNum = do
        word8' hash
        w <- parseEntityHex <|> parseEntityDig
        return $ ContentText $ pack [toEnum w]
    parseEntityHex = do
        word8' charx
        res <- hexadecimal
        word8' semicolon
        return res
    hexadecimal = do
        x <- hex
        hexadecimal' $ fromIntegral x
    hexadecimal' x = (do
        y <- hex
        hexadecimal' $ x * 16 + fromIntegral y
        ) <|> return x
    hex = satisfyWith hex' (< 16)
    hex' w
        | char0 <= w && w <= char9 = w - char0
        | charA <= w && w <= charZ = w - charA + 10
        | chara <= w && w <= charz = w - chara + 10
        | otherwise = 16 -- failing case
    parseEntityDig = do
        res <- decimal
        word8' semicolon
        return res
    decimal = do
        x <- dig
        decimal' $ fromIntegral x
    decimal' x = (do
        y <- dig
        decimal' $ x * 10 + fromIntegral y
        ) <|> return x
    dig = satisfyWith dig' (< 10)
    dig' w
        | char0 <= w && w <= char9 = w - char0
        | otherwise = 10 -- failing case
    parseEntityWord = do
        s <- takeWhile1 (/= semicolon)
        word8' semicolon
        return $ case s of
            _
                | s == "amp"  -> ContentText "&"
                | s == "gt"   -> ContentText ">"
                | s == "lt"   -> ContentText "<"
                | s == "apos" -> ContentText "'"
                | s == "quot" -> ContentText "\""
                | otherwise   ->
                    ContentEntity $ toText s
    parseText = do
        bs <- takeWhile1 valid
        return $ ContentText $ toText bs
    valid 34 = not breakDouble
    valid 39 = not breakSingle
    valid 38 = False -- amp
    valid 60 = False -- lt
    valid _  = True

toText :: S.ByteString -> Text
toText = fromChunks . return
       . TS.replace "\r\n" "\n" -- FIXME do this more efficiently
       . decodeUtf8With lenientDecode

skipSpace :: Parser ()
skipSpace = skipWhile isSpaceW

newline :: Parser ()
newline = ((word8 13 >> word8 10) <|> word8 10) >> return ()

word8' :: Word8 -> Parser ()
word8' c = word8 c >> return ()

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
tag' :: Monad m
     => Name
     -> AttrParser a
     -> (a -> Iteratee SEvent m b)
     -> Iteratee SEvent m (Maybe b)
tag' name attrParser = tag
    (\x -> if x == name then Just () else Nothing)
    (const attrParser)

-- | A further simplified tag parser, which requires that no attributes exist.
tag'' :: Monad m => Name -> Iteratee SEvent m a -> Iteratee SEvent m (Maybe a)
tag'' name f = tag' name (return ()) $ const f

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
        $ detectUtf       $$ joinI
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
