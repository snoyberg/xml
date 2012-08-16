{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.DTD.Cache
import qualified Text.XML as X
import qualified Text.XML.Unresolved as XU
import Control.Monad
import Filesystem.Path.CurrentOS (decodeString)

main :: IO ()
main = do
    (catfile:files) <- getArgs
    cache <- newDTDCacheFile catfile
    forM_ files $ \file -> do
        doc <- XU.readFile X.def $ decodeString file
        let file' = decodeString $ file ++ ".applied"
        applyDTD_ cache doc >>= X.writeFile X.def file'
