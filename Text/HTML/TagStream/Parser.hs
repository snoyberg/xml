{-# LANGUAGE OverloadedStrings, TupleSections, PatternGuards #-}
module Text.HTML.TagStream.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec.Char8 hiding (inClass)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Text.HTML.TagStream.Types
import Text.HTML.TagStream.Utils (cons, append)

cond :: a -> a -> Bool -> a
cond a1 a2 b = if b then a1 else a2

(||.) :: Applicative f => f Bool -> f Bool -> f Bool
(||.) = liftA2 (||)

inClass :: Eq a => [a] -> a -> Bool
inClass (b:c:[]) a = a==b || a==c
inClass xs x = x `elem` xs

takeTillN :: Int -> (Char -> Bool) -> Parser ByteString
takeTillN 0 p = takeTill p
takeTillN n p = S.cons <$> satisfy (not . p)
                       <*> takeTillN (n-1) p

boolP :: Parser a -> Parser Bool
boolP p = p *> pure True <|> pure False

{--
 - match quoted string, single or double quote, can fail.
 -}
quoted :: Parser (Char, ByteString)
quoted = do
    q <- satisfy (inClass "'\"")
    s <- str q
    return (q, s)
  where
    str q = append <$> takeTill ((=='\\') ||. (==q))
                   <*> (end q <|> unescape q)
    end q = char q *> pure ""
    unescape q = char '\\' *>
                 (cons <$> anyChar <*> str q)

{--
 - attribute value, can't fail.
 -}
attrValue :: Parser ByteString
attrValue = snd <$> quoted
    <|> takeTill (inClass ">" ||. isSpace)

{--
 - attribute name, at least one char, can fail when meet tag end.
 - might match self-close tag end "/>" , make sure match `tagEnd' first.
 -}
attrName :: Parser ByteString
attrName = snd <$> quoted
   <|> cons <$> satisfy (not . inClass ">")
            <*> takeTill (inClass "/>=" ||. isSpace)

{--
 - tag end, return self-close or not, can fail.
 -}
tagEnd :: Parser Bool
tagEnd = char '>' *> pure False
     <|> string "/>" *> pure True

{--
 - attribute pair or tag end, can fail if tag end met.
 -}
attr :: Parser Attr
attr = do
    n <- attrName
    skipSpace
    v <- char '=' *> skipSpace *> attrValue
         <|> pure ""
    return (n, v)

{--
 - all attributes before tag end. can't fail.
 -}
attrs :: Parser ([Attr], Bool)
attrs = loop []
  where
    loop acc = skipSpace *> (Left <$> tagEnd <|> Right <$> attr) >>=
               either
                 (return . (reverse acc,))
                 (loop . (:acc))

{--
 - comment tag without prefix.
 -}
comment :: Parser Token
comment = Comment <$> comment'
  where comment' = append <$> takeTill (=='-')
                          <*> ( string "-->" *> return ""
                                <|> cons <$> anyChar <*> comment' )

{--
 - tags begine with <! , e.g. <!DOCTYPE ...>
 -}
special :: Parser Token
special = Special
          <$> ( cons
                <$> satisfy (not . ((=='-') ||. isSpace))
                <*> takeTill ((=='>') ||. isSpace)
                <* skipSpace )
          <*> takeTill (=='>') <*  char '>'

{--
 - parse a tag, can fail.
 -}
tag :: Parser Token
tag = do
    t <- string "</"     *> return TagTypeClose
         <|> string "<!" *> return TagTypeSpecial
         <|> char '<'    *> return TagTypeNormal
    case t of
        TagTypeClose ->
            TagClose <$> takeTill (=='>')
            <* char '>'
        TagTypeSpecial -> boolP (string "--") >>=
                          cond comment special
        TagTypeNormal -> do
            name <- cons <$> satisfy (not . (isSpace ||. (inClass "<>")))
                         <*> takeTill (inClass "/>" ||. isSpace)
            (as, close) <- attrs
            skipSpace
            return $ TagOpen name as close

{--
 - parse text node. consume at least one char, to make sure progress.
 -}
text :: Parser Token
text = Text <$> ( cons <$> anyChar <*> takeTill (=='<') )

token :: Parser Token
token = tag <|> text

html :: Parser [Token]
html = many token

decode :: ByteString -> Either String [Token]
decode = parseOnly html
