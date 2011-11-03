{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Text.HTML.TagStream.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Text.HTML.TagStream.Types

(||.) :: Applicative f => f Bool -> f Bool -> f Bool
(||.) = liftA2 (||)
-- (&&.) = liftA2 (&&)

value :: Parser ByteString
value = char '"' *> str
    <|> takeTill (inClass "/>=" ||. isSpace)
  where
    str = S.append <$> takeTill (inClass "\\\"") 
                   <*> (end <|> unescape)
    end = char '"' *> return ""
    unescape = char '\\' *> 
               (S.cons <$> anyChar <*> str)

attr :: Parser Attr
attr = do
    skipSpace
    c <- satisfy (notInClass "/>")
    name' <- takeTill (inClass ">=" ||. isSpace)
    let name = S.cons c name'
    skipSpace
    option (name, S.empty) $ do
        _ <- char '='
        skipSpace
        (name,) <$> value

attrs :: Parser [Attr]
attrs = many attr

comment :: Parser ByteString
comment = S.append <$>
            takeTill (=='-') <*>
            ( string "-->" *> return "" <|>
              S.cons <$> anyChar <*> comment )

tag :: Parser Token
tag = Comment <$> ( string "<!--" *> comment )
  <|> TagClose <$> ( string "</" *>
                     takeTill (=='>') <*
                     char '>' )
  <|> TagOpen <$> ( char '<' *>
                    ( S.cons <$>
                        satisfy (not . (isSpace ||. (inClass "!>"))) <*>
                        takeTill (inClass "/>=" ||. isSpace)) )
              <*> attrs <* skipSpace
              <*> ( char '>' *> return False <|>
                    string "/>" *> return True )

text :: Parser Token
text = Text <$> (
         S.cons <$> anyChar <*> takeTill (=='<')
       )

token :: Parser Token
token = tag <|> text

html :: Parser [Token]
html = many token

decode :: ByteString -> Either String [Token]
decode = parseOnly html
