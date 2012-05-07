{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Text.HTML.TagStream.Types where

import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Blaze.ByteString.Builder (Builder, fromByteString, toByteString)
import Control.Arrow ((***))

type Attr' s = (s, s)
type Attr = Attr' ByteString
data Token' s = TagOpen s [Attr' s] Bool
              | TagClose s
              | Text s
              | Comment s
              | Special s s
              | Incomplete s
    deriving (Eq, Show)

data TagType = TagTypeClose
             | TagTypeSpecial
             | TagTypeNormal

type Token = Token' ByteString

cc :: [ByteString] -> Builder
cc = mconcat . map fromByteString

showToken :: (ByteString -> ByteString) -> Token -> Builder
showToken hl (TagOpen name as close) =
    cc $ [hl "<", name]
      ++ map showAttr as
      ++ [hl (if close then "/>" else ">")]
  where
    showAttr :: Attr -> ByteString
    showAttr (key, value) = S.concat $ [" ", key, hl "=\""] ++ map escape (S.unpack value) ++ [hl "\""]
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = S.singleton c
showToken hl (TagClose name) = cc [hl "</", name, hl ">"]
showToken _ (Text s) = fromByteString s
showToken hl (Comment s) = cc [hl "<!--", s, hl "-->"]
showToken hl (Special name s) = cc [hl "<!", name, " ", s, hl ">"]
showToken _ (Incomplete s) = fromByteString s

encode :: [Token] -> ByteString
encode = encodeHL id

encodeHL :: (ByteString -> ByteString) -> [Token] -> ByteString
encodeHL hl = toByteString . mconcat . map (showToken hl)

instance Functor Token' where
    fmap f (TagOpen x pairs b) = TagOpen (f x) (map (f *** f) pairs) b
    fmap f (TagClose x) = TagClose (f x)
    fmap f (Text x) = Text (f x)
    fmap f (Comment x) = Comment (f x)
    fmap f (Special x y) = Special (f x) (f y)
    fmap f (Incomplete x) = Incomplete (f x)
