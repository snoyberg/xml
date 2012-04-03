{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Text.HTML.TagStream.Stream where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.ByteString (parseOnly)
import Data.Conduit
import Text.HTML.TagStream.Parser
import Text.HTML.TagStream.Types

-- | html parser conduit.
tokenStream :: Monad m => Conduit ByteString m Token
tokenStream = conduitState S.empty push close
  where
    push accum input =
        case parseOnly html (accum `S.append` input) of
            Right (splitAccum -> (accum', tokens)) -> return $ StateProducing accum' tokens
            Left err -> fail err

    close s = return $ if S.null s then [] else [Text s]

    splitAccum :: [Token] -> (ByteString, [Token])
    splitAccum [] = (S.empty, [])
    splitAccum (reverse -> (Incomplete s : xs)) = (s, reverse xs)
    splitAccum tokens = (S.empty, tokens)
