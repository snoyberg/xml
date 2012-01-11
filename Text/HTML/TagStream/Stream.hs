{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Text.HTML.TagStream.Stream where

import Data.ByteString (ByteString)
import Data.Attoparsec.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S
import Data.Conduit
import Text.HTML.TagStream.Parser
import Text.HTML.TagStream.Types

-- | like concatMap, with a accumerator.
--
-- Since 0.0.0
tokenStream :: Resource m => Conduit ByteString m Token
tokenStream = conduitState S.empty push close
  where
    push accum input =
        case parseOnly html (accum `S.append` input) of
            Left err -> fail err
            Right (splitAccum -> (accum', tokens)) -> return (accum', Producing tokens)

    close s = return $ if S.null s then [] else [Text s]

    splitAccum :: [Token] -> (ByteString, [Token])
    splitAccum [] = (S.empty, [])
    splitAccum (reverse -> (Incomplete s : xs)) = (s, reverse xs)
    splitAccum tokens = (S.empty, tokens)
