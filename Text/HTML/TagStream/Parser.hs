{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Text.HTML.TagStream.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Text.HTML.TagStream.Types
import Text.HTML.TagStream.Utils (cons, append)

(||.) :: Applicative f => f Bool -> f Bool -> f Bool
(||.) = liftA2 (||)
-- (&&.) = liftA2 (&&)

value :: Parser ByteString
value = (satisfy (inClass "'\"") >>= str)
    <|> takeTill (inClass ">=" ||. isSpace)
  where
    str q = append <$> takeTill ((=='\\') ||. (==q))
                   <*> (end q <|> unescape q)
    end q = char q *> return ""
    unescape q = char '\\' *>
                 (cons <$> anyChar <*> str q)

attr :: Parser Attr
attr = do
    skipSpace
    c <- satisfy (notInClass "/>")
    name' <- takeTill (inClass ">=" ||. isSpace)
    let name = cons c name'
    skipSpace
    option (name, S.empty) $ do
        _ <- char '='
        skipSpace
        (name,) <$> value

attrs :: Parser [Attr]
attrs = many attr

comment :: Parser ByteString
comment = append <$>
            takeTill (=='-') <*>
            ( string "-->" *> return "" <|>
              cons <$> anyChar <*> comment )

special :: Parser Token
special = Comment <$> ( string "--" *> comment )
      <|> Special
          <$> ( cons
                <$> satisfy (not . ((=='-') ||. isSpace))
                <*> takeTill ((=='>') ||. isSpace)
                <* skipSpace )
          <*> takeTill (=='>')
          <*  char '>'

tag :: Parser Token
tag = string "<!" *> special
  <|> string "</"
      *> (TagClose <$> takeTill (=='>'))
      <* char '>'
  <|> char '<'
      *> ( TagOpen
           <$> ( cons
                 <$> satisfy (not . (isSpace ||. (inClass "!>")))
                 <*> takeTill (inClass "/>" ||. isSpace) )
           <*> attrs <* skipSpace
           <*> ( char '>' *> return False
             <|> string "/>" *> return True ) )

tagClose :: ByteString -> Parser [Token]
tagClose name = string (S.concat ["</", name, ">"]) *> return [TagClose name]

textTill :: Parser [Token] -> Parser [Token]
textTill p = (:) <$> text <*> (p <|> textTill p)

text :: Parser Token
text = Text <$> (
         cons <$> anyChar <*> takeTill (=='<')
       )

token :: Parser Token
token = tag <|> text

html :: Parser [Token]
html = html' <|> return []
  where html' = do
            x <- token
            xs <- case x of
                    TagOpen name _ False
                      | name `elem` ["script", "style"]
                        -> ( (++) <$> (let p=tagClose name in p <|> textTill p)
                                  <*> html )
                           <|> return []
                    _ -> html
            return (x:xs)

decode :: ByteString -> Either String [Token]
decode = parseOnly html
