{-# LANGUAGE DeriveDataTypeable #-}
module Network.URI.Enumerator.ZIP
    ( zipScheme
    , ZipException (..)
    , toArchive
    , readZipFile
    ) where

import Network.URI.Enumerator
import Codec.Archive.Zip
import Data.Text (Text, snoc, unpack)
import qualified Data.Set as Set
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Enumerator (throwError, enumList)
import qualified Data.ByteString.Lazy as L

data ZipException = FileNotFound Text
    deriving (Show, Typeable)
instance Exception ZipException

zipScheme :: Text -- ^ scheme name
          -> Archive
          -> Scheme
zipScheme name archive = Scheme
    { schemeNames = Set.singleton $ snoc name ':'
    , schemeReader = Just $ \uri step ->
        case findEntryByPath (unpack $ uriPath uri) archive of
            Nothing -> throwError $ FileNotFound $ uriPath uri
            Just e -> enumList 8 (L.toChunks $ fromEntry e) step
    , schemeWriter = Nothing
    }

readZipFile :: FilePath -> IO Archive
readZipFile = fmap toArchive . L.readFile
