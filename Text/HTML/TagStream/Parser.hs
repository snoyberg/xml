{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Text.HTML.TagStream.Parser where

import Control.Applicative ( (<$>), (<*), (<|>) )
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Text.HTML.TagStream.Types

-- TODO handle escape double-quote
value :: Parser ByteString
value = do
    c <- anyChar
    case c of
        '"' -> takeTill (=='"') <* anyChar
        _   -> S.cons c <$> takeTill (inClass ">= ")

attr :: Parser Attr
attr = do
    skipSpace
    c <- satisfy (/='>')
    name' <- takeTill (inClass ">= ")
    let name = S.cons c name'
    skipSpace
    option (name, S.empty) $ do
        _ <- char '='
        skipSpace
        (name,) <$> value
        -- (name,) <$> takeTill (inClass ">= ")

attrs :: Parser [Attr]
attrs = many attr

tag :: Parser Token
tag = do
    _ <- char '<'
    skipSpace
    name <- takeTill (inClass "> ")
    c <- anyChar
    case c of
        '>' -> case S.uncons name of
                   Just ('/', name') -> return (TagClose name')
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
