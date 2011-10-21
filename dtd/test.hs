{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.DTD.Cache
import qualified Text.XML as X
import qualified Text.XML.Unresolved as XU
import qualified Data.ByteString.Lazy as L
import Control.Monad

main :: IO ()
main = do
    (catfile:files) <- getArgs
    cache <- newDTDCacheFile catfile
    forM_ files $ \file -> do
        doc <- XU.readFile_ X.def file
        let file' = file ++ ".applied"
        applyDTD_ cache doc >>= X.writeFile X.def file'
