{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
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
-- > import Text.XML.Enumerator.Parse
-- > import Data.Text.Lazy (Text, unpack)
-- > 
-- > data Person = Person { age :: Int, name :: Text }
-- >     deriving Show
-- > 
-- > parsePerson = tagName "person" (requireAttr "age") $ \age -> do
-- >     name <- content
-- >     return $ Person (read $ unpack age) name
-- > 
-- > parsePeople = tagNoAttr "people" $ many parsePerson
-- > 
-- > main = parseFile_ "people.xml" decodeEntities $ force "people required" parsePeople
--
-- will produce:
--
-- > [Person {age = 25, name = "Michael"},Person {age = 2, name = "Eliezer"}]
module Text.XML.Enumerator.Parse
    ( -- * Parsing XML files
      parseBytes
    , parseText
    , detectUtf
    , parseFile
    , parseFile_
    , parseLBS
    , parseLBS_
      -- ** Entity decoding
    , DecodeEntities
    , decodeEntities
      -- * Event parsing
    , tag
    , tagName
    , tagNoAttr
    , content
    , contentMaybe
    , processElem
    , processSiblings
    , ignoreElem
    , ignoreSiblings
      -- * Attribute parsing
    , AttrParser
    , requireAttr
    , optionalAttr
    , requireAttrRaw
    , optionalAttrRaw
    , ignoreAttrs
    , skipAttrs
    , parseAttrsT
    , parseAttrsS
    , parseAttrsST
      -- * Combinators
    , choose
    , many
    , force
    , skipTill
    , skipSiblings
      -- * Exceptions
    , XmlException (..)
    ) where
import Data.Attoparsec.Text
    ( char, Parser, takeWhile1, skipWhile, string
    , manyTill, takeWhile, try, anyChar, endOfInput
    )
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text.Enumerator (iterParser)
import Data.XML.Types
    ( Name (..), Event (..), Content (..)
    , Instruction (..), ExternalID (..)
    )
import Control.Applicative ((<|>), (<$>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML.Enumerator.Token
import Prelude hiding (takeWhile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.Enumerator
    ( Iteratee, Enumeratee, (>>==), Stream (..), run_, Enumerator, Step (..)
    , checkDone, yield, ($$), joinI, run, throwError, returnI
    )
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
import Control.Monad(liftM2)
import Control.Arrow((***))
import Data.Char (isSpace)
import Data.Maybe(fromJust)

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
    fixAttName level (name', val) = (tnameToName True level name', val)
    begin = EventBeginElement (tnameToName False l' name)
          $ Map.fromList
          $ map (fixAttName l')
          $ as' []
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
tokenToEvent n (TokenDoctype t eid) = (n, [EventBeginDoctype t eid, EventEndDoctype])
tokenToEvent n (TokenCDATA t) = (n, [EventCDATA t])

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
parseBytes :: Monad m => DecodeEntities -> Enumeratee S.ByteString Event m a
parseBytes de step = joinI $ detectUtf $$ parseText de step

-- | Parses a character stream into 'Event's. This function is implemented
-- fully in Haskell using attoparsec-text for parsing. The produced error
-- messages do not give line/column information, so you may prefer to stick
-- with the parser provided by libxml-enumerator. However, this has the
-- advantage of not relying on any C libraries.
parseText :: Monad m => DecodeEntities -> Enumeratee TS.Text Event m a
parseText de =
    checkDone $ \k -> k (Chunks [EventBeginDocument]) >>== loop []
  where
    loop levels = checkDone $ go levels
    go levels k = do
        mtoken <- iterToken de
        case mtoken of
            Nothing -> k (Chunks [EventEndDocument]) >>== return
            Just token ->
                let (levels', events) = tokenToEvent levels token
                 in k (Chunks events) >>== loop levels'

iterToken :: Monad m => DecodeEntities -> Iteratee TS.Text m (Maybe Token)
iterToken de = iterParser ((endOfInput >> return Nothing) <|> fmap Just (parseToken de))

parseToken :: DecodeEntities -> Parser Token
parseToken de = do
    (char '<' >> parseLt) <|> fmap TokenContent (parseContent de False False)
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
        return $ TokenBeginElement n as isClose

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
contentMaybe :: Monad m => Iteratee Event m (Maybe Text)
contentMaybe = do
    x <- E.peek
    case pc' x of
        Ignore -> EL.drop 1 >> contentMaybe
        IsContent t -> EL.drop 1 >> fmap Just (takeContents (t:))
        IsError e -> throwError $ XmlException e x
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
    pc EventDeclaration{} = Ignore
    pc EventEndDoctype = Ignore
    pc EventInstruction{} = Ignore
    pc EventComment{} = Ignore
    takeContents front = do
        x <- E.peek
        case pc' x of
            Ignore -> EL.drop 1 >> takeContents front
            IsContent t -> EL.drop 1 >> takeContents (front . (:) t)
            IsError e -> throwError $ XmlException e x
            NotContent -> return $ T.concat $ front []

-- | Grabs the next piece of content. If none if available, returns 'T.empty'.
-- This is simply a wrapper around 'contentMaybe'.
content :: Monad m => Iteratee Event m Text
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
tag :: Monad m
    => (Name -> Maybe a)
    -> (a -> AttrParser b)
    -> (b -> Iteratee Event m c)
    -> Iteratee Event m (Maybe c)
tag checkName attrParser f = do
    x <- dropWS
    case x of
        Just (EventBeginElement name as) ->
            case checkName name of
                Just y ->
                    case runAttrParser' (attrParser y) $ Map.toList as of
                        Left e -> throwError e
                        Right z -> do
                            EL.drop 1
                            z' <- f z
                            a <- dropWS
                            case a of
                                Just (EventEndElement name')
                                    | name == name' -> EL.drop 1 >> return (Just z')
                                _ -> throwError $ XmlException ("Expected end tag for: " ++ show name) a
                Nothing -> return Nothing
        _ -> return Nothing
  where
    dropWS = do
        x <- E.peek
        let isWS =
                case x of
                    Just EventBeginDocument -> True
                    Just EventEndDocument -> True
                    Just EventBeginDoctype{} -> True
                    Just EventDeclaration{} -> True
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
        if isWS then EL.drop 1 >> dropWS else return x
    runAttrParser' p as =
        case runAttrParser p as of
            Left e -> Left e
            Right ([], x) -> Right x
            Right (attr, _) -> Left $ UnparsedAttributes attr

-- | A simplified version of 'tag' which matches for specific tag names instead
-- of taking a predicate function. This is often sufficient, and when combined
-- with OverloadedStrings and the IsString instance of 'Name', can prove to be
-- very concise.
tagName :: Monad m
     => Name
     -> AttrParser a
     -> (a -> Iteratee Event m b)
     -> Iteratee Event m (Maybe b)
tagName name attrParser = tag
    (\x -> if x == name then Just () else Nothing)
    (const attrParser)

-- | A further simplified tag parser, which requires that no attributes exist.
tagNoAttr :: Monad m => Name -> Iteratee Event m a -> Iteratee Event m (Maybe a)
tagNoAttr name f = tagName name (return ()) $ const f

-- | Get the value of the first parser which returns 'Just'. If no parsers
-- succeed (i.e., return 'Just'), this function returns 'Nothing'.
choose :: Monad m
       => [Iteratee Event m (Maybe a)]
       -> Iteratee Event m (Maybe a)
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
      -> Iteratee Event m (Maybe a)
      -> Iteratee Event m a
force msg i = do
    x <- i
    case x of
        Nothing -> throwError $ XmlException msg Nothing
        Just a -> return a

-- | The same as 'parseFile', but throws any exceptions.
parseFile_ :: FilePath -> DecodeEntities -> Iteratee Event IO a -> IO a
parseFile_ fn de p =
    parseFile fn de p >>= go
  where
    go (Left e) = liftIO $ throwIO e
    go (Right a) = return a

-- | A helper function which reads a file from disk using 'enumFile', detects
-- character encoding using 'detectUtf', parses the XML using 'parseBytes', and
-- then hands off control to your supplied parser.
parseFile :: FilePath
          -> DecodeEntities
          -> Iteratee Event IO a
          -> IO (Either SomeException a)
parseFile fn de p =
    run $ enumFile fn     $$ joinI
        $ parseBytes de   $$ p

-- | Parse an event stream from a lazy 'L.ByteString'.
parseLBS :: L.ByteString -> DecodeEntities -> Iteratee Event IO a -> IO (Either SomeException a)
parseLBS lbs de p =
    run $ enumSingle (L.toChunks lbs)   $$ joinI
        $ parseBytes de                 $$ p

-- | Same as 'parseLBS', but throws exceptions.
parseLBS_ :: L.ByteString -> DecodeEntities -> Iteratee Event IO a -> IO a
parseLBS_ lbs de p =
    run_ $ enumSingle (L.toChunks lbs)   $$ joinI
         $ parseBytes de                 $$ p

enumSingle :: Monad m => [a] -> Enumerator a m b
enumSingle as (Continue k) = k $ Chunks as
enumSingle _ step = returnI step

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
many :: Monad m => Iteratee Event m (Maybe a) -> Iteratee Event m [a]
many i =
    go id
  where
    go front = do
        x <- i
        case x of
            Nothing -> return $ front []
            Just y -> go $ front . (:) y

-- Internal Iteratee used in processSiblings and processElem
processNested :: (Monad m) => Bool -> Iteratee Event m b -> Iteratee Event m (Maybe b)
processNested isElem k0 = E.continue (loop (Just []) k0)
    where
        loop :: (Monad m) => Maybe [Name] -> Iteratee Event m b -> Stream Event -> Iteratee Event m (Maybe b)
        loop ns k (Chunks xs) = 
            case (isElem, skipNames ns [] xs) of
                (_, (Nothing, _, _)) -> E.yield Nothing (Chunks xs)
                (True, (Just [], ts, xs')) -> yield ts xs'
                (False, (ns', ts, [])) -> continue ns' ts
                (False, (Just [], ts, xs')) -> yield ts xs'
                (True, (ns', ts, [])) -> continue ns' ts
                (_, (Just [n1,n2], _, _)) -> throwError $ XmlException ("Unbalanced xml-tree. Name '" ++ show n1 ++ "' is not corresponding to '"  
                                                    ++ show n2 ++ "'. (Error in skipSiblings)") (Just $ EventEndElement n2)
                _ -> throwError $ XmlException "Unknown error. (Error in skipSiblings)" Nothing
            where
                continue ns' ts = E.continue (loop ns' $ E.enumList 1 ts $$ k)
                yield ts xs' = do
                    t <- E.Iteratee $ liftM (flip E.Yield (Chunks [])) $ E.run $ E.enumList 1 ts $$ k
                    case t of
                        Left err -> throwError err
                        Right t' -> E.yield (Just t') (Chunks xs')

                skipNames :: Maybe [Name] -> [Event] -> [Event] -> (Maybe [Name], [Event], [Event])
                skipNames ns0 ts0 es0 = go ns0 ts0 es0
                    where
                        go ns ts [] = (ns,ts,[])
                        go Nothing _ _ = (ns0,ts0,es0)
                        go (Just ns) ts xxs@(x:xs) = 
                            case x of
                                (EventBeginElement n _) -> go (Just $ n:ns) (ts ++ [x]) xs
                                (EventEndElement n)
                                    | isElem && null (tail ns) -> (Just [], ts ++ [x], xs)
                                    | isElem && null ns -> (Nothing, ts, xxs)
                                    | not isElem && null ns -> (Just [], ts, xxs)
                                    -- | null (if isElem then tail ns else ns) -> (Just [], if isElem then ts ++ [x] else ts, (f xxs))
                                    | n == head ns -> go (Just $ tail ns) (ts ++ [x]) xs
                                    | otherwise -> (Just [head ns, n], ts, xxs)
                                _ -> go (Just ns) (ts ++ [x]) xs
        loop _ _ EOF = throwError $ XmlException "Unbalanced xml-tree. (Error in skipSiblings - EOF)" Nothing
        
                    
-- | Iteratee to process sibling elements in separate iteratee.
processSiblings :: (Monad m) => Iteratee Event m b -> Iteratee Event m b
processSiblings = liftM fromJust . processNested False

-- | Iteratee to process sibling elements in separate iteratee.
processElem :: (Monad m) => Iteratee Event m b -> Iteratee Event m (Maybe b)
processElem = processNested True
    
iterIgnore :: (Monad m) => Iteratee a m ()
iterIgnore = E.returnI $ E.Yield () EOF

-- | Iteratee to skip sibling elements.
ignoreSiblings :: Monad m => Iteratee Event m ()
ignoreSiblings = processSiblings iterIgnore
   
-- | Iteratee to skip the next element. Skips all events before the next
-- element as well. Returns False if an element's end event is encountered
-- before any element's begin events.
ignoreElem :: Monad m => Iteratee Event m Bool
ignoreElem = liftM (maybe False $ const True) $ processElem iterIgnore

-- | Skip the sibling elements until iteratee returns 'Just'.
skipTill :: Monad m => Iteratee Event m (Maybe a) -> Iteratee Event m (Maybe a)
skipTill i = go
    where
        go = i >>= \x -> case x of
            Nothing -> ignoreElem >>= (\b -> if b then go else return Nothing)
            _ -> return x

-- | Combinator to skip the siblings element. 
skipSiblings :: Monad m => Iteratee Event m a -> Iteratee Event m a
skipSiblings i = i >>= \r -> ignoreSiblings >> return r

-- | Combinator to skip the attributes.
skipAttrs :: AttrParser a -> AttrParser a
skipAttrs i = i >>= \r -> ignoreAttrs >> return r

-- | Simple AttrParser utilities
-- | Parse list of required and list of optional attributes into lists of Text
parseAttrsT :: [Name] -> [Name] -> AttrParser ([Text], [Maybe Text])
parseAttrsT reqs opts = liftM2 (,) (mapM requireAttr reqs) (mapM optionalAttr opts)

-- | Parse list of required and list of optional attributes into lists of String
parseAttrsS :: [Name] -> [Name] -> AttrParser ([String], [Maybe String])
parseAttrsS reqs opts = fmap (map T.unpack *** map (fmap T.unpack)) (parseAttrsT reqs opts)

-- | Parse part of atrributes into [String] and other part into [Text]
parseAttrsST :: [Name] -> [Name] -> [Name] -> [Name] -> AttrParser (([String], [Maybe String]), ([Text], [Maybe Text]))
parseAttrsST reqs opts reqst optst = liftM2 (,) (parseAttrsS reqs opts) (parseAttrsT reqst optst)

type DecodeEntities = Text -> Content

-- | Default implementation of 'DecodeEntities': handles numeric entities and
-- the five standard character entities (lt, gt, amp, quot, apos).
decodeEntities :: DecodeEntities
decodeEntities "lt" = ContentText "<"
decodeEntities "gt" = ContentText ">"
decodeEntities "amp" = ContentText "&"
decodeEntities "quot" = ContentText "\""
decodeEntities "apos" = ContentText "'"
decodeEntities t =
    case T.uncons t of
        Just ('#', t') ->
            case T.uncons t' of
                Just ('x', t'') -> decodeHex (ContentEntity t) t''
                _ -> decodeDec (ContentEntity t) t'
        _ -> ContentEntity t

decodeHex :: Content -> Text -> Content
decodeHex backup val
    | T.null val = backup
decodeHex backup val =
    go (T.unpack val) 0
  where
    go [] i = ContentText $ T.singleton $ toEnum i
    go (c:cs) i = maybe backup (go cs . ((i * 16) +)) $ getHex c
    getHex c
        | '0' <= c && c <= '9' = Just $ fromEnum c - fromEnum '0'
        | 'A' <= c && c <= 'F' = Just $ fromEnum c - fromEnum 'A' + 10
        | 'a' <= c && c <= 'f' = Just $ fromEnum c - fromEnum 'a' + 10
        | otherwise = Nothing

decodeDec :: Content -> Text -> Content
decodeDec backup val
    | T.null val = backup
decodeDec backup val =
    go (T.unpack val) 0
  where
    go [] i = ContentText $ T.singleton $ toEnum i
    go (c:cs) i = maybe backup (go cs . ((i * 10) +)) $ getHex c
    getHex c
        | '0' <= c && c <= '9' = Just $ fromEnum c - fromEnum '0'
        | otherwise = Nothing
