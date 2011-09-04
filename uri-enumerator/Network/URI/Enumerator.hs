{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.URI.Enumerator
    ( -- * Base datatypes
      URI (..)
    , URIAuth (..)
      -- * Parsing
    , parseURI
    , parseURIReference
    , parseRelativeReference
      -- * Utils
    , nullURI
    , hasExtension
    , relativeTo
      -- * Conversion
    , toNetworkURI
    , fromNetworkURI
      -- * Perform I/O
    , Scheme (..)
    , SchemeMap
    , toSchemeMap
    , readURI
    , writeURI
    , copyURI
    ) where

import qualified Network.URI as N
import Data.Text (Text, cons, isSuffixOf, pack, unpack)
import Data.Enumerator (Enumerator, run_, ($$), throwError)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO)
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import Data.Enumerator.Binary (enumFile, iterHandle)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Failure (Failure (..))
import Control.Exception (Exception)
import Data.Typeable (Typeable)

data URI = URI
    { uriScheme :: Text
    , uriAuthority :: Maybe URIAuth
    , uriPath :: Text
    , uriQuery :: Text
    , uriFragment :: Text
    }
    deriving (Show, Eq, Ord)

data URIAuth = URIAuth
    { uriUserInfo :: Text
    , uriRegName :: Text
    , uriPort :: Text
    }
    deriving (Show, Eq, Ord)

parseURI :: Failure URIException m => Text -> m URI
parseURI t = maybe (failure $ InvalidURI t) (return . fromNetworkURI) $ N.parseURI $ unpack t

parseURIReference :: Failure URIException m => Text -> m URI
parseURIReference t = maybe (failure $ InvalidURI t) (return .  fromNetworkURI) $ N.parseURIReference $ unpack t

parseRelativeReference :: Failure URIException m => Text -> m URI
parseRelativeReference t = maybe (failure $ InvalidRelativeReference t) (return . fromNetworkURI) $ N.parseRelativeReference $ unpack t

hasExtension :: URI -> Text -> Bool
hasExtension URI { uriPath = p } t = (cons '.' t) `isSuffixOf` p

data Scheme m = Scheme
    { schemeNames :: Set.Set Text
    , schemeReader :: forall b. Maybe (URI -> Enumerator ByteString m b)
    , schemeWriter :: Maybe (URI -> Enumerator ByteString m () -> m ())
    }

type SchemeMap m = Map.Map Text (Scheme m)

toSchemeMap :: [Scheme m] -> SchemeMap m
toSchemeMap =
    Map.unions . map go
  where
    go s =
        Map.unions $ map go' $ Set.toList $ schemeNames s
      where
        go' name = Map.singleton name s

data URIException = UnknownReadScheme URI
                  | UnknownWriteScheme URI
                  | InvalidURI Text
                  | InvalidRelativeReference Text
  deriving (Show, Typeable)
instance Exception URIException

readURI :: Monad m => SchemeMap m -> URI -> Enumerator ByteString m b
readURI sm uri step =
    case Map.lookup (uriScheme uri) sm >>= schemeReader of
        Nothing -> throwError $ UnknownReadScheme uri
        Just f -> f uri step

writeURI :: Failure URIException m => SchemeMap m -> URI -> Enumerator ByteString m () -> m ()
writeURI sm uri enum =
    case Map.lookup (uriScheme uri) sm >>= schemeWriter of
        Nothing -> failure $ UnknownWriteScheme uri
        Just f -> f uri enum

toNetworkURI :: URI -> N.URI
toNetworkURI u = N.URI
    { N.uriScheme = unpack $ uriScheme u
    , N.uriAuthority = fmap go $ uriAuthority u
    , N.uriPath = unpack $ uriPath u
    , N.uriQuery = unpack $ uriQuery u
    , N.uriFragment = unpack $ uriFragment u
    }
  where
    go a = N.URIAuth
        { N.uriUserInfo = unpack $ uriUserInfo a
        , N.uriRegName = unpack $ uriRegName a
        , N.uriPort = unpack $ uriPort a
        }

fromNetworkURI :: N.URI -> URI
fromNetworkURI u = URI
    { uriScheme = pack $ N.uriScheme u
    , uriAuthority = fmap go $ N.uriAuthority u
    , uriPath = pack $ N.uriPath u
    , uriQuery = pack $ N.uriQuery u
    , uriFragment = pack $ N.uriFragment u
    }
  where
    go a = URIAuth
        { uriUserInfo = pack $ N.uriUserInfo a
        , uriRegName = pack $ N.uriRegName a
        , uriPort = pack $ N.uriPort a
        }

relativeTo :: URI -> URI -> Maybe URI
relativeTo a b = fmap fromNetworkURI $ toNetworkURI a `N.relativeTo` toNetworkURI b

nullURI :: URI
nullURI = fromNetworkURI N.nullURI

copyURI :: Failure URIException m => SchemeMap m -> URI -> URI -> m ()
copyURI sm src dst = writeURI sm dst $ readURI sm src
