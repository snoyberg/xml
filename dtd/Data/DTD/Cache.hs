{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Data.DTD.Cache
    ( DTDCache
    , applyDTD
    , newDTDCache
    , newDTDCacheFile
    ) where

import qualified Text.XML as X
import qualified Data.Map as Map
import Data.Text (Text)
import Text.XML.Catalog
import Network.URI.Enumerator
import Network.URI.Enumerator.File
import qualified Data.DTD.Types as D
import Data.DTD.Parse (readEID, uriToEID)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.IORef as I
import Control.Exception (Exception, throwIO, SomeException)
import Data.Typeable (Typeable)
import Data.Maybe (mapMaybe)
import Control.Monad.IO.Control (MonadControlIO)
import qualified Network.URI as NU

toAttrs :: [D.DTDComponent] -> AttrMap
toAttrs comps =
    Map.fromList $ mapMaybe go comps
  where
    go (D.DTDAttList (D.AttList lname atts)) = Just $ (X.Name lname Nothing Nothing, mapMaybe go' atts)
    go _ = Nothing
    go' (D.AttDecl lname _ def) =
        case def of
            D.AttFixed t -> Just (name, Fixed t)
            D.AttDefaultValue t -> Just (name, Def t)
            _ -> Nothing
      where
        name = X.Name lname Nothing Nothing

data DTDCache = DTDCache
    { _dcCache :: I.IORef (Map.Map PubSys AttrMap)
    , _dcCatalog :: Catalog
    , _dcSchemeMap :: forall m. MonadControlIO m => SchemeMap m
    }

newDTDCache :: MonadIO m' => Catalog -> (forall m. MonadControlIO m => SchemeMap m) -> m' DTDCache
newDTDCache c sm = do
    x <- liftIO $ I.newIORef Map.empty
    return $ DTDCache x c sm

newDTDCacheFile :: MonadControlIO m => FilePath -> m DTDCache
newDTDCacheFile fp = do
    uri <- liftIO $ decodeString fp
    c <- loadCatalog (toSchemeMap [fileScheme]) uri
    newDTDCache c (toSchemeMap [fileScheme])

loadDTD :: MonadControlIO m => DTDCache -> X.ExternalID -> m AttrMap
loadDTD (DTDCache icache catalog sm) ext = do
    res <- liftIO $ fmap (Map.lookup pubsys) $ I.readIORef icache
    case res of
        Just dtd -> return dtd
        Nothing ->
            case Map.lookup pubsys catalog of
                Nothing -> liftIO $ throwIO $ UnknownExternalID ext
                Just uri -> do
                    ecomps <- E.run $ readEID catalog (uriToEID uri) sm E.$$ EL.consume
                    comps <- either (liftIO . throwIO . CannotLoadDTD (toNetworkURI uri)) return ecomps
                    let attrMap = toAttrs comps
                    liftIO $ I.atomicModifyIORef icache $ \m ->
                        (Map.insert pubsys attrMap m, ())
                    return attrMap
  where
    pubsys =
        case ext of
            X.SystemID t -> System t
            X.PublicID t _ -> Public t

data UnknownExternalID = UnknownExternalID X.ExternalID
                       | CannotLoadDTD NU.URI SomeException
    deriving (Show, Typeable)
instance Exception UnknownExternalID

applyDTD :: MonadControlIO m
         => DTDCache -> X.Document -> m X.Document
applyDTD dc doc@(X.Document pro@(X.Prologue _ mdoctype _) root epi) =
    case mdoctype of
        Just (X.Doctype _ (Just extid)) -> do
            attrs <- loadDTD dc extid
            let root' = go attrs root
            return $ X.Document pro root' epi
        _ -> return doc
  where
    go attrs (X.Element name as ns) =
        X.Element name as' ns'
      where
        as' =
            case Map.lookup name attrs of
                Nothing -> as
                Just x -> foldr goa as x
        ns' = map gon ns
        gon (X.NodeElement e) = X.NodeElement $ go attrs e
        gon n = n
    goa (name, Fixed t) as = (name, t) : filter (\(n, _) -> name /= n) as
    goa (name, Def t) as =
        case lookup name as of
            Nothing -> (name, t) : as
            Just _ -> as

data Att = Def Text | Fixed Text

type AttrMap = Map.Map X.Name [(X.Name, Att)]
