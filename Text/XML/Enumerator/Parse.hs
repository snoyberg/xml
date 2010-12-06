{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Enumerator.Parse
    ( parseToken
    , parseBytes
    , detectUtf
    ) where
import Data.Attoparsec
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
import Data.Map (Map)
import Data.Enumerator (Iteratee, Enumeratee, (>>==), Stream (..),
                        checkDone, yield, Step, ($$), joinI, (==<<))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Text as E
import Control.Monad (unless)
import qualified Data.Text as ST

amp, hash, charx, semicolon, char0, char9, charA, charZ, chara, charz
   , colon, equal, squote, dquote, lt, gt, qmark, fslash, exmark, dash
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

tokenToEvent :: [NSLevel] -> Token -> ([NSLevel], [Event])
tokenToEvent n (TokenBeginDocument _) = (n, [])
tokenToEvent n (TokenInstruction i) = (n, [EventInstruction i])
tokenToEvent n (TokenBeginElement name as isClosed) =
    (n', if isClosed then [begin, end] else [begin])
  where
    l0 = case n of
            [] -> NSLevel Nothing Map.empty
            x:_ -> x
    (as', l') = foldr go (id, l0) as
    go a@(TName kpref kname, val) (front, l)
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
    fixAttName level (name', val) = Attribute (tnameToName level name') val
    begin = EventBeginElement (tnameToName l' name) $ map (fixAttName l') $ as' []
    end = EventEndElement $ tnameToName l' name
tokenToEvent n (TokenEndElement name) =
    (n', [EventEndElement $ tnameToName l name])
  where
    (l, n') =
        case n of
            [] -> (NSLevel Nothing Map.empty, [])
            x:xs -> (x, xs)
tokenToEvent n (TokenContent c) = (n, [EventContent c])
tokenToEvent n (TokenComment c) = (n, [EventComment c])
tokenToEvent n (TokenDoctype t eid) = (n, [EventDoctype $ Doctype t eid []])

tnameToName (NSLevel def _) (TName Nothing name) = Name name def Nothing
tnameToName (NSLevel _ m) (TName (Just pref) name) =
    case Map.lookup pref m of
        Just ns -> Name name (Just ns) (Just pref)
        Nothing -> Name name Nothing (Just pref) -- FIXME is this correct?

detectUtf :: Monad m => Enumeratee S.ByteString S.ByteString m a
detectUtf param = do
    x <- takeFourBytes S.empty
    let (toDrop, mcodec) =
            case S.unpack x of
                [0x00, 0x00, 0xFE, 0xFF] -> (4, Just E.utf32_be)
                [0xFF, 0xFE, 0x00, 0x00] -> (4, Just E.utf32_le)
                0xFE : 0xFF: _           -> (2, Just E.utf16_be)
                0xFF : 0xFE: _           -> (2, Just E.utf16_le)
                0xEF : 0xBB: 0xBF : _    -> (3, Nothing)
                [0x00, 0x00, 0x00, 0x3C] -> (0, Just E.utf32_be)
                [0x3C, 0x00, 0x00, 0x00] -> (0, Just E.utf32_le)
                [0x00, 0x3C, 0x00, 0x3F] -> (0, Just E.utf16_be)
                [0x3C, 0x00, 0x3F, 0x00] -> (0, Just E.utf16_le)
                _                        -> (0, Nothing) -- Assuming UTF-8
    unless (toDrop == 4) $ yield () $ Chunks [S.drop toDrop x]
    x <- E.peek
    iter <-
      case mcodec of
        Nothing -> return param
        Just codec -> (joinI $ E.decode codec $$ joinI $ E.encode E.utf8 param) >>== return
    E.map id iter
  where
    takeFourBytes front = do
        x <- E.head
        case x of
            Nothing -> return front
            Just y -> do
                let z = S.append front y
                if S.length z < 4
                    then takeFourBytes z
                    else do
                        let (a, b) = S.splitAt 4 z
                        E.yield a $ Chunks [b]

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

data NSLevel = NSLevel
    { defaultNS :: Maybe Text
    , prefixes :: Map Text Text
    }
    deriving Show

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
                as <- many parseAttribute
                skipSpace
                word8 qmark
                word8 gt
                newline
                return $ TokenBeginDocument as
            else do
                skipSpace
                x <- toText <$> takeWhile (/= qmark)
                word8 gt
                return $ TokenInstruction $ Instruction name x
    parseComment = do
        word8 dash
        word8 dash
        c <- toText . S.pack <$> manyTill anyWord8 (string "-->") -- FIXME use takeWhile instead
        return $ TokenComment c
    parseCdata = do
        string "[CDATA["
        t <- toText . S.pack <$> manyTill anyWord8 (string "]]>") -- FIXME use takeWhile instead
        return $ TokenContent $ ContentText t
    parseDoctype = do
        string "DOCTYPE"
        skipSpace
        i <- parseIdent
        skipSpace
        eid <- fmap Just parsePublicID <|>
               fmap Just parseSystemID <|>
               return Nothing
        skipSpace
        word8 gt
        newline
        return $ TokenDoctype i eid
    parsePublicID = do
        string "PUBLIC"
        x <- quotedText
        y <- quotedText
        return $ PublicID x y
    parseSystemID = do
        string "SYSTEM"
        x <- quotedText
        return $ SystemID x
    quotedText = do
        skipSpace
        toText <$> (between dquote <|> between squote)
    between c = do
        word8 c
        x <- takeWhile (/=c)
        word8 c
        return x
    parseEnd = do
        skipSpace
        n <- parseName
        skipSpace
        word8 gt
        return $ TokenEndElement n
    parseBegin = do
        skipSpace
        n <- parseName
        as <- many parseAttribute
        skipSpace
        isClose <- (word8 fslash >> skipSpace >> return True) <|> return False
        word8 gt
        return $ TokenBeginElement n as isClose

parseAttribute :: Parser TAttribute
parseAttribute = do
    skipSpace
    key <- parseName
    skipSpace
    word8 equal
    skipSpace
    val <- squoted <|> dquoted
    return (key, val)
  where
    squoted = do
        word8 squote
        manyTill (parseContent False True) (word8 squote)
    dquoted = do
        word8 dquote
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
    valid c  = not $ isSpace c

isSpace :: Word8 -> Bool
isSpace 0x09 = True
isSpace 0x0A = True
isSpace 0x0D = True
isSpace 0x20 = True
isSpace _    = False

parseContent :: Bool -- break on double quote
             -> Bool -- break on single quote
             -> Parser Content
parseContent breakDouble breakSingle =
    parseEntity <|> parseText
  where
    parseEntity = do
        word8 amp
        parseEntityNum <|> parseEntityWord
    parseEntityNum = do
        word8 hash
        w <- parseEntityHex <|> parseEntityDig
        return $ ContentText $ pack [toEnum w]
    parseEntityHex = do
        word8 charx
        res <- hexadecimal
        word8 semicolon
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
        word8 semicolon
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

toText = fromChunks . return . decodeUtf8With lenientDecode
skipSpace = skipWhile isSpace
newline = (word8 13 >> word8 10) <|> word8 10
