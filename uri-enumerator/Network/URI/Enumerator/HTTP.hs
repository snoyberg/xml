{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.URI.Enumerator.HTTP
    ( httpScheme
    , withManager
    ) where

import Network.URI.Enumerator (Scheme (..), toNetworkURI)
import Network.HTTP.Enumerator (Manager, parseUrl, httpRedirect, withManager)
import Network.HTTP.Types (Status, status200)
import Data.Enumerator (throwError, returnI)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Set as Set
import Control.Monad.Trans.Class (lift)

data HTTPURIException = NotOKResponse Status
    deriving (Show, Typeable)
instance Exception HTTPURIException

httpScheme :: Manager -> Scheme IO -- FIXME
httpScheme m = Scheme
    { schemeNames = Set.fromList ["http:", "https:"]
    , schemeReader = Just $ \uri step -> do
        req <- lift $ parseUrl $ show $ toNetworkURI uri
        flip (httpRedirect req) m $ \status _ ->
            if status == status200
                then returnI step
                else throwError $ NotOKResponse status
    , schemeWriter = Nothing
    }
