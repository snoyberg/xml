{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.URI.Conduit.File
    ( decodeString
    , fileScheme
    , toFilePath
    ) where

import Prelude hiding (catch, FilePath)
import Network.URI (unEscapeString)
import Network.URI.Conduit
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Filesystem as F
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S

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
            parseURI $ T.append "file://" $ T.concatMap fixSpace $ T.map fixSlash $ either id id $ FP.toText fp
  where
    fixSlash '\\' = '/'
    fixSlash c = c
    fixSpace ' ' = "%20"
    fixSpace c = T.singleton c

fileScheme :: Scheme
fileScheme = Scheme
    { schemeNames = Set.singleton "file:"
    , schemeReader = Just $ sourceFile . toFilePath
    , schemeWriter = Just $ \uri -> C.Sink $ do
        let fp = toFilePath uri
        liftIO $ F.createTree $ FP.directory fp
        C.prepareSink $ CB.sinkFile (FP.encodeString fp)
    }

toFilePath :: URI -> FP.FilePath
toFilePath uri = FP.fromText $
    case uriAuthority uri of
        Nothing -> uriPath uri
        Just a -> T.concat [uriRegName a, uriPort a, T.pack $ unEscapeString $ T.unpack $ uriPath uri]

sourceFile :: C.ResourceIO m => FP.FilePath -> C.Source m S.ByteString
sourceFile = CB.sourceFile . FP.encodeString
