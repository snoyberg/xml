{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.URI.Conduit.HTTP
    ( httpScheme
    , withManager
    ) where

import Network.URI.Conduit (Scheme (..), toNetworkURI, URI)
import Data.ByteString (ByteString)
import Network.HTTP.Conduit
import Network.HTTP.Types (Status, status200)
import Data.Conduit
import Data.Conduit.Internal (sourceToPipe)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Set as Set
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (register)

httpScheme :: Manager -> Scheme
httpScheme m = Scheme
    { schemeNames = Set.fromList ["http:", "https:"]
    , schemeReader = Just (httpReader m)
    , schemeWriter = Nothing
    }

--httpReader :: Manager -> URI -> Pipe l i ByteString u (ResourceT IO) ()
httpReader m uri = do
    req <- liftIO $ parseUrl $ show $ toNetworkURI uri
    res <- lift $ http req m
    (src, final) <- lift $ unwrapResumable $ responseBody res
    sourceToPipe src
    lift final
