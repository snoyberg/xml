{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | HTML entity decoding.

module Text.HTML.TagStream.Entities
  (Dec(..)
  ,isNameChar
  ,isNameStart
  ,decodeEntities)
  where

import Data.Char
import Data.Monoid
import Data.String
import Data.Conduit
import Text.HTML.TagStream.Types
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe, isJust)
import Control.Arrow (first)

decodeEntities' :: (Monad m
                   ,Monoid string
                   ,IsString string
                   ,Monoid builder
                   ,Eq string)
                => Dec builder string
                -> Conduit string m string
decodeEntities' dec =
    loop id
  where
    loop accum = do
        mchunk <- await
        let chunk = accum $ fromMaybe mempty mchunk
            (newStr, remainder) = makeEntityDecoder dec chunk
        yield newStr
        if isJust mchunk
            then loop (mappend remainder)
            else yield remainder

decodeEntities :: (Monad m
                  ,Monoid builder
                  ,Monoid string
                  ,IsString string
                  ,Eq string)
               => Dec builder string
               -> Conduit (Token' string) m (Token' string)
decodeEntities dec =
    start
  where
    start = await >>= maybe (return ()) (\token -> start' token >> start)
    start' (Text t) = (yield t >> yieldWhileText) =$= decodeEntities' dec =$= CL.mapMaybe go
    start' token = yield token

    go t
        | t == ""   = Nothing
        | otherwise = Just (Text t)

yieldWhileText :: Monad m => Conduit (Token' string) m string
yieldWhileText =
    loop
  where
    loop = await >>= maybe (return ()) go
    go (Text t) = yield t >> loop
    go token = leftover token

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
makeEntityDecoder :: (IsString string,Monoid builder,Eq string)
                  => Dec builder string -> string -> (string, string)
makeEntityDecoder Dec{..} = first decToS . go
  where
    go s =
      case decBreak (=='&') s of
        (_,"") -> (decBuilder s, "")
        (before,restPlusAmp@(decDrop 1 -> rest)) ->
          case decBreak (not . (\c -> isNameChar c || c == '#')) rest of
            (_,"") -> (decBuilder before, restPlusAmp)
            (entity,after) ->
                let before1 = decBuilder before
                    decoded = decBuilder (decEntity entity)
                    (before2, after') =
                        case decUncons after of
                            Just (';',validAfter) -> first (decoded <>) (go validAfter)
                            Just (_invalid,_rest) -> first (decoded <>) (go after)
                            Nothing -> (mempty, s)
                 in (before1 <> before2, after')

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
