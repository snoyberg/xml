{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Text.HTML.TagStream.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

type Attr' s = (s, s)
type Attr = Attr' ByteString
data Token' s = TagOpen s [Attr' s] Bool
              | TagClose s
              | Text s
              | Comment s
              | Special s s
    deriving (Eq, Show)

type Token = Token' ByteString

showToken :: (ByteString -> ByteString) -> Token -> ByteString
showToken hl (TagOpen name as close) =
    S.concat $ [hl "<", name]
            ++ map showAttr as
            ++ [hl (if close then "/>" else ">")]
  where
    showAttr :: Attr -> ByteString
    showAttr (key, value) = S.concat [" ", key, hl "=\"", S.pack . concatMap escape . S.unpack $ value, hl "\""]
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = [c]
showToken hl (TagClose name) = S.concat [hl "</", name, hl ">"]
showToken _ (Text s) = s
showToken hl (Comment s) = S.concat [hl "<!--", s, hl "-->"]
showToken hl (Special name s) = S.concat [hl "<!", name, " ", s, hl ">"]

encode :: [Token] -> ByteString
encode = encodeHL id

encodeHL :: (ByteString -> ByteString) -> [Token] -> ByteString 
encodeHL hl = S.concat . map (showToken hl)
