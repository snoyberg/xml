{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.URI.Enumerator.File
    ( decodeString
    , fileScheme
    , toFilePath
    , enumFile
    ) where

import Prelude hiding (catch)
import Network.URI (unEscapeString)
import Network.URI.Enumerator
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Enumerator (run_, ($$), Enumerator, tryIO, Iteratee (..))
import Data.Enumerator.Binary (iterHandle, enumHandle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.IO as SIO
import Data.ByteString (ByteString)
import Control.Exception.Lifted (bracket, finally)
import Control.Monad.Trans.Control (MonadBaseControl)

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
    , schemeReader = Just $ \uri step -> do
        let fp = toFilePath uri
        enumFile fp step
    , schemeWriter = Just $ \uri enum -> do
        let fp = toFilePath uri
        liftIO $ F.createTree $ FP.directory fp
        withFile fp F.WriteMode $ \h -> run_ $ enum $$ iterHandle h
    }

withFile :: (MonadIO m, MonadBaseControl IO m) => FP.FilePath -> F.IOMode -> (SIO.Handle -> m a) -> m a
withFile fp mode = bracket (liftIO $ SIO.openBinaryFile (FP.encodeString fp) mode) $ liftIO . SIO.hClose

enumFile :: (MonadIO m, MonadBaseControl IO m) => FP.FilePath -> Enumerator ByteString m a
enumFile fp step = do
    h <- tryIO $ SIO.openBinaryFile (FP.encodeString fp) SIO.ReadMode
    let iter = enumHandle 4096 h step
    -- FIXME this isn't truly safe
    Iteratee (finally (runIteratee iter) (liftIO $ SIO.hClose h))

toFilePath :: URI -> FP.FilePath
toFilePath uri = FP.fromText $
    case uriAuthority uri of
        Nothing -> uriPath uri
        Just a -> T.concat [uriRegName a, uriPort a, T.pack $ unEscapeString $ T.unpack $ uriPath uri]
