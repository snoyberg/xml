{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HTML entity decoding.

module Text.HTML.TagStream.Entities
  (Dec(..)
  ,makeEntityDecoder
  ,isNameChar
  ,isNameStart)
  where

import Data.Char
import Data.Monoid
import Data.String

-- | A decoder.
data Dec builder string = Dec
  { decToS     :: builder -> string
  , decBreak   :: (Char -> Bool) -> string -> (string,string)
  , decBuilder :: string -> builder
  , decDrop    :: Int -> string -> string
  , decEntity  :: string -> string
  , decUncons  :: string -> Maybe (Char,string)
  }

-- | Decode the entities in a string type with a decoder.
makeEntityDecoder :: (Eq string,IsString string,Monoid builder)
                  => Dec builder string -> string -> string
makeEntityDecoder Dec{..} = decToS . go
  where
    go s =
      case decBreak (=='&') s of
        (_,"") -> decBuilder s
        (before,decDrop 1 -> rest) ->
          case decBreak (not . (\c -> isNameChar c || c == '#')) rest of
            (_,"") -> decBuilder before <> decBuilder rest
            (entity,after) ->
              decBuilder before <>
              decBuilder (decEntity entity) <>
              go (case decUncons after of
                    Just (';',validAfter) -> validAfter
                    _ -> after)

-- | Is the character a valid Name starter?
isNameStart :: Char -> Bool
isNameStart c =
  c == ':' ||
  c == '_' ||
  isAsciiUpper c ||
  isAsciiLower c ||
  (c >= '\xC0' && c <= '\xD6') ||
  (c >= '\xD8' && c <= '\xF6') ||
  (c >= '\xF8' && c <= '\x2FF') ||
  (c >= '\x370' && c <= '\x37D') ||
  (c >= '\x37F' && c <= '\x1FFF') ||
  (c >= '\x200C' && c <= '\x200D') ||
  (c >= '\x2070' && c <= '\x218F') ||
  (c >= '\x2C00' && c <= '\x2FEF') ||
  (c >= '\x3001' && c <= '\xD7FF') ||
  (c >= '\xF900' && c <= '\xFDCF') ||
  (c >= '\xFDF0' && c <= '\xFFFD') ||
  (c >= '\x10000' && c <= '\xEFFFF')

-- | Is the character valid in a Name?
isNameChar :: Char -> Bool
isNameChar c =
  c == '-' ||
  c == '.' ||
  c == '\xB7' ||
  isDigit c ||
  isNameStart c ||
  (c >= '\x0300' && c <= '\x036F') ||
  (c >= '\x203F' && c <= '\x2040')
