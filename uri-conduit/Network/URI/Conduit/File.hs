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
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.IO as SIO
import Data.ByteString (ByteString)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Filesystem as F

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

fileScheme :: Scheme
fileScheme = Scheme
    { schemeNames = Set.singleton "file:"
    , schemeReader = Just $ CB.sourceFile . toFilePath
    , schemeWriter = Just $ \uri -> C.SinkM $ do
        let fp = toFilePath uri
        C.liftBase $ F.createTree $ FP.directory fp
        C.genSink $ CB.sinkFile fp
    }

toFilePath :: URI -> FP.FilePath
toFilePath uri = FP.fromText $
    case uriAuthority uri of
        Nothing -> uriPath uri
        Just a -> T.concat [uriRegName a, uriPort a, T.pack $ unEscapeString $ T.unpack $ uriPath uri]
