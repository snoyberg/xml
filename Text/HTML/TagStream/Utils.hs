{-# LANGUAGE ViewPatterns #-}
module Text.HTML.TagStream.Utils where

import Data.Monoid (Monoid(..))
import Text.HTML.TagStream.Types

splitAccum :: Monoid s => [Token' s] -> (s, [Token' s])
splitAccum [] = (mempty, [])
splitAccum (reverse -> (Incomplete s : xs)) = (s, reverse xs)
splitAccum tokens = (mempty, tokens)
