{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Text.HTML.TagStream.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

type Attr' s = (s, s)
type Attr = Attr' ByteString
data Token' s = TagOpen s [Attr' s]
              | TagClose s
              | Text s
--    deriving (Show)

type Token = Token' ByteString

instance Show (Token' ByteString) where
    show (TagOpen name as) = "TagOpen "++S.unpack name++show as
    show (TagClose name) = "TagClose "++S.unpack name
    show (Text s) = "Text"++show (S.length s)

showToken :: Token -> ByteString
showToken (TagOpen name as) = S.concat $ ["<", name] ++ map showAttr as ++ [">"]
  where
    showAttr :: Attr -> ByteString
    showAttr (key, value) = S.concat [" ", key, "=\"", value, "\""]

showToken (TagClose name) = S.concat ["</", name, ">"]
showToken (Text s) = s
