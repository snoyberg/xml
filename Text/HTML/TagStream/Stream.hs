{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Text.HTML.TagStream.Stream where

import Control.Monad (liftM)
import Data.ByteString (ByteString)
import Data.Attoparsec.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Text.HTML.TagStream.Parser
import Text.HTML.TagStream.Types

accumParse :: Monad m => ByteString -> ByteString -> m (ByteString, [Token])
accumParse acc input = liftM splitAccum $
                         either fail return $
                           parseOnly html (acc `S.append` input)
  where
    splitAccum :: [Token] -> (ByteString, [Token])
    splitAccum [] = (S.empty, [])
    splitAccum (reverse -> (Text s : xs)) = (s, reverse xs)
    splitAccum tokens = (S.empty, tokens)

tokenStream :: Monad m => E.Enumeratee ByteString Token m b
tokenStream = EL.concatMapAccumM accumParse S.empty
