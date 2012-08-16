{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.URI.Enumerator.HTTP
    ( httpScheme
    , withManager
    ) where

import Network.URI.Enumerator (Scheme (..), toNetworkURI)
import Network.HTTP.Enumerator (Manager, parseUrl, httpRedirect, withManager, HttpException)
import Network.HTTP.Types (Status, status200)
import Data.Enumerator (throwError, returnI)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Set as Set
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Failure (Failure)

data HTTPURIException = NotOKResponse Status
    deriving (Show, Typeable)
instance Exception HTTPURIException

httpScheme :: Manager -> Scheme
httpScheme m = Scheme
    { schemeNames = Set.fromList ["http:", "https:"]
    , schemeReader = Just $ \uri step -> do
        req <- liftIO $ parseUrl $ show $ toNetworkURI uri
        flip (httpRedirect req) m $ \status _ ->
            if status == status200
                then returnI step
                else throwError $ NotOKResponse status
    , schemeWriter = Nothing
    }
