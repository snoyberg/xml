{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.URI.Conduit
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
      -- * Exception
    , URIException (..)
    ) where

import qualified Network.URI as N
import Data.Text (Text, cons, isSuffixOf, pack, unpack)
import qualified Data.Conduit as C
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Failure (Failure (..))
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceIO)
import Control.Monad.Trans.Class (lift)

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

data Scheme = Scheme
    { schemeNames :: Set.Set Text
    , schemeReader :: forall m. ResourceIO m => Maybe (URI -> C.SourceM m ByteString)
    , schemeWriter :: forall m. ResourceIO m => Maybe (URI -> C.SinkM ByteString m ())
    }

type SchemeMap = Map.Map Text Scheme

toSchemeMap :: [Scheme] -> SchemeMap
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

readURI :: ResourceIO m
        => SchemeMap
        -> URI
        -> C.SourceM m ByteString
readURI sm uri = C.SourceM $ do
    case Map.lookup (uriScheme uri) sm >>= schemeReader of
        Nothing -> lift $ C.resourceThrow $ UnknownReadScheme uri
        Just f -> C.genSource $ f uri

writeURI :: ResourceIO m
         => SchemeMap
         -> URI
         -> C.SinkM ByteString m ()
writeURI sm uri =
    case Map.lookup (uriScheme uri) sm >>= schemeWriter of
        Nothing -> lift $ C.resourceThrow $ UnknownWriteScheme uri
        Just f -> f uri

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

copyURI :: ResourceIO m
        => SchemeMap
        -> URI
        -> URI
        -> m ()
copyURI sm src dst = C.runResourceT $ readURI sm src C.$$ writeURI sm dst
