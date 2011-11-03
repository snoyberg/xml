{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Text.HTML.TagStream.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

type Attr' s = (s, s)
type Attr = Attr' ByteString
data Token' s = TagOpen s [Attr' s] Bool
              | TagClose s
              | Text s
    deriving (Eq, Show)

type Token = Token' ByteString

-- instance Show (Token' ByteString) where
--     show (TagOpen name as) = "TagOpen "++S.unpack name++show as
--     show (TagClose name) = "TagClose "++S.unpack name
--     show (Text s) = "Text"++show (S.length s)

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

encode :: [Token] -> ByteString
encode = encodeHL id

encodeHL :: (ByteString -> ByteString) -> [Token] -> ByteString 
encodeHL hl = S.concat . map (showToken hl)
