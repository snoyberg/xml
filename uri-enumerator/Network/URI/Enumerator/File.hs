{-# LANGUAGE OverloadedStrings #-}
module Network.URI.Enumerator.File
    ( decodeString
    , fileScheme
    ) where

import Network.URI.Enumerator
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Enumerator (run_, ($$))
import Data.Enumerator.Binary (enumFile, iterHandle)
import Data.Maybe (fromMaybe)

-- | Converts a string, such as a command-line argument, into a URI. First
-- tries to parse as an absolute URI. If this fails, it interprets as a
-- relative or absolute filepath.
decodeString :: String -> IO URI
decodeString s =
    case parseURI $ T.pack s of
        Just u -> return u
        Nothing -> do
            wd <- F.getWorkingDirectory
            let fp = wd FP.</> FP.decodeString s
            parseURI $ T.append "file://" $ T.map fixSlash $ either id id $ FP.toText fp
  where
    fixSlash '\\' = '/'
    fixSlash c = c

fileScheme :: Scheme IO -- FIXME not just IO
fileScheme = Scheme
    { schemeNames = Set.singleton "file:"
    , schemeReader = Just $ \uri step -> do
        let fp = toFilePath uri
        enumFile (FP.encodeString fp) step
    , schemeWriter = Just $ \uri enum -> do
        let fp = toFilePath uri
        F.createTree $ FP.directory fp
        F.withFile fp F.WriteMode $ \h -> run_ $ enum $$ iterHandle h
    }

toFilePath :: URI -> FP.FilePath
toFilePath uri = FP.fromText $ fromMaybe "" (fmap uriRegName $ uriAuthority uri) `T.append` uriPath uri
