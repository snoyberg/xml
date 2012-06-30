{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Text.HTML.TagStream.Stream where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.ByteString (parseOnly)
import Data.Conduit
import Text.HTML.TagStream.Parser
import Text.HTML.TagStream.Types
import Control.Monad (unless)

-- | html parser conduit.
tokenStream :: Monad m => GInfConduit ByteString m Token
tokenStream =
    loop S.empty
  where
    loop accum = awaitE >>= either (close accum) (push accum)

    push accum input =
        case parseOnly html (accum `S.append` input) of
            Right (splitAccum -> (accum', tokens)) -> mapM_ yield tokens >> loop accum'
            Left err -> fail err

    close s r = do
        unless (S.null s) $ yield $ Text s
        return r

    splitAccum :: [Token] -> (ByteString, [Token])
    splitAccum [] = (S.empty, [])
    splitAccum (reverse -> (Incomplete s : xs)) = (s, reverse xs)
    splitAccum tokens = (S.empty, tokens)
