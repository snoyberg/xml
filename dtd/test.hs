{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.DTD.Cache
import qualified Text.XML as X
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    [catfile, file] <- getArgs
    doc <- X.readFile_ X.def file
    cache <- newDTDCacheFile catfile
    applyDTD cache doc >>= L.putStr . X.renderLBS X.def
