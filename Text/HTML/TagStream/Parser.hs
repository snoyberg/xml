{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Text.HTML.TagStream.Parser where

import Prelude hiding (takeWhile)
import Debug.Trace
import Control.Applicative ( (<$>), (<*>), (<*), (<|>) )
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Text.HTML.TagStream.Types

f `or'` g = \a -> f a || g a

value :: Parser ByteString
value = do
    c <- anyChar
    case c of
        '"' -> unescape
        _   -> S.cons c <$> takeTill (inClass ">=" `or'` isSpace)
  where
    unescape = do
        v <- takeTill (inClass "\\\"")
        c <- anyChar
        case c of
            '"' -> return v
            '\\' -> S.append v <$> (S.cons <$> anyChar <*> unescape)

attr :: Parser Attr
attr = do
    skipSpace
    c <- satisfy (/='>')
    name' <- takeTill (inClass ">=" `or'` isSpace)
    let name = S.cons c name'
    skipSpace
    option (name, S.empty) $ do
        _ <- char '='
        skipSpace
        (name,) <$> value

attrs :: Parser [Attr]
attrs = many attr

-- TODO self close tag
tag :: Parser Token
tag = do
    _ <- char '<'
    spaces <- takeWhile isSpace
    name' <- takeTill (inClass "<>=" `or'` isSpace)
    let name = S.append spaces name'
    c <- satisfy (isSpace `or'` (=='>'))
    case c of
        '>' -> case S.uncons name of
                   Just ('/', name'') -> return (TagClose name'')
                   _ -> return (TagOpen name [])
        _   -> TagOpen name <$> attrs <* skipSpace <* char '>'

text :: Parser Token
text = do
    c <- anyChar
    s <- takeTill (=='<')
    return $ Text $ S.cons c s

token :: Parser Token
token = tag <|> text

html :: Parser [Token]
html = many token

decode :: ByteString -> Either String [Token]
decode = parseOnly html
