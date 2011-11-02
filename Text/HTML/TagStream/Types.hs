{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Text.HTML.TagStream.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

type Attr' s = (s, s)
type Attr = Attr' ByteString
data Token' s = TagOpen s [Attr' s]
              | TagClose s
              | Text s
    deriving (Eq, Show)

type Token = Token' ByteString

-- instance Show (Token' ByteString) where
--     show (TagOpen name as) = "TagOpen "++S.unpack name++show as
--     show (TagClose name) = "TagClose "++S.unpack name
--     show (Text s) = "Text"++show (S.length s)

showToken :: (ByteString -> ByteString) -> Token -> ByteString
showToken highlight (TagOpen name as) = S.concat $ [highlight "<", name] ++ map showAttr as ++ [highlight ">"]
  where
    showAttr :: Attr -> ByteString
    showAttr (key, value) = S.concat [" ", key, highlight "=\"", S.pack . concatMap escape . S.unpack $ value, highlight "\""]
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = [c]
showToken highlight (TagClose name) = S.concat [highlight "</", name, highlight ">"]
showToken highlight (Text s) = s

encode :: [Token] -> ByteString
encode = encodeHL id

encodeHL :: (ByteString -> ByteString) -> [Token] -> ByteString 
encodeHL hl = S.concat . map (showToken hl)
