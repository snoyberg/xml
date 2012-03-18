{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.DTD.Cache
    ( DTDCache
    , applyDTD
    , applyDTD_
    , newDTDCache
    , newDTDCacheFile
    , loadAttrMap
    , UnresolvedEntity (..)
    ) where

import qualified Text.XML as X
import Text.XML.Cursor
import qualified Data.XML.Types as XU
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML.Catalog
import Network.URI.Conduit
import Network.URI.Conduit.File
import qualified Data.DTD.Types as D
import Data.DTD.Parse (readEID, uriToEID)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.IORef as I
import Control.Exception (Exception, throwIO, SomeException)
import Data.Typeable (Typeable)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Network.URI as NU
import Control.Exception.Lifted (try)
import Control.Monad.Trans.Resource (ResourceIO)
import Control.Monad (liftM)

toMaps :: [D.DTDComponent] -> (EntityMap, AttrMap)
toMaps =
    foldr go (Map.empty, Map.empty)
  where
    go (D.DTDEntityDecl (D.InternalGeneralEntityDecl k v)) (e, a) = (Map.insert k v e, a)
    go (D.DTDAttList (D.AttList lname atts)) (e, a) =
        (e, Map.unionWith Map.union (Map.singleton (X.Name lname Nothing Nothing) (Map.unions $ map go' atts)) a)
    go _ m = m

    go' (D.AttDecl lname _ def) =
        case def of
            D.AttFixed t -> Map.singleton name $ Fixed t
            D.AttDefaultValue t -> Map.singleton name $ Def t
            _ -> Map.empty
      where
        name = X.Name lname Nothing Nothing

data DTDCache = DTDCache
    { _dcCache :: I.IORef (Map.Map PubSys (EntityMap, AttrMap))
    , _dcCatalog :: Catalog
    , _dcSchemeMap :: SchemeMap
    }

newDTDCache :: MonadIO m' => Catalog -> SchemeMap -> m' DTDCache
newDTDCache c sm = do
    x <- liftIO $ I.newIORef Map.empty
    return $ DTDCache x c sm

newDTDCacheFile :: ResourceIO m => FilePath -> m DTDCache
newDTDCacheFile fp = do
    uri <- liftIO $ decodeString fp
    c <- loadCatalog (toSchemeMap [fileScheme]) uri
    newDTDCache c (toSchemeMap [fileScheme])

loadSchemaAttrMap :: ResourceIO m => DTDCache -> Text -> m (EntityMap, AttrMap)
loadSchemaAttrMap (DTDCache icache catalog sm) uri0 = do
    res <- liftIO $ fmap (Map.lookup pubsys) $ I.readIORef icache
    case res of
        Just dtd -> return dtd
        Nothing -> do
            res' <- load uri0
            let maps = (Map.empty, res')
            liftIO $ I.atomicModifyIORef icache $ \m ->
                (Map.insert pubsys maps m, ())
            return maps
  where
    pubsys = Public uri0

    load uri =
        case resolveURI catalog Nothing (X.PublicID uri uri) of
            Nothing -> C.resourceThrow $ UnknownSchemaURI uri
            Just uri' -> do
                doc <- C.runResourceT $ readURI sm uri' C.$$ X.sinkDoc X.def
                let c = fromDocument doc
                let includes =
                        (c $// element "{http://www.w3.org/2001/XMLSchema}include" >=> attribute "schemaLocation") ++
                        (c $// element "{http://www.w3.org/2001/XMLSchema}redefine" >=> attribute "schemaLocation")
                ms1 <- mapM load includes
                let ms2 = c $// element "{http://www.w3.org/2001/XMLSchema}element" >=> go
                let ms = ms1 ++ map (uncurry Map.singleton) ms2
                return $ Map.unionsWith Map.union ms

    go c = do
        name <- attribute "name" c
        let attrs = c $// element "{http://www.w3.org/2001/XMLSchema}attribute" >=> goA
        return (X.Name name Nothing Nothing, Map.fromList attrs)

    goA c = do
        ref <- attribute "ref" c
        def <- attribute "default" c
        return (X.Name ref Nothing Nothing, Def def)

loadAttrMap :: ResourceIO m => DTDCache -> X.ExternalID -> m (EntityMap, AttrMap)
loadAttrMap (DTDCache icache catalog sm) ext = do
    res <- liftIO $ fmap (Map.lookup pubsys) $ I.readIORef icache
    case res of
        Just dtd -> return dtd
        Nothing ->
            case Map.lookup pubsys catalog of
                Nothing -> C.resourceThrow $ UnknownExternalID ext
                Just uri -> do
                    ecomps <- try $ C.runResourceT $ readEID catalog (uriToEID uri) sm C.$$ CL.consume
                    comps <- either (liftIO . throwIO . CannotLoadDTD (toNetworkURI uri)) return ecomps
                    let maps = toMaps comps
                    liftIO $ I.atomicModifyIORef icache $ \m ->
                        (Map.insert pubsys maps m, ())
                    return maps
  where
    pubsys =
        case ext of
            X.SystemID t -> System t
            X.PublicID t _ -> Public t

data UnknownExternalID = UnknownExternalID X.ExternalID
                       | CannotLoadDTD NU.URI SomeException
                       | UnknownSchemaURI Text
    deriving (Show, Typeable)
instance Exception UnknownExternalID

data UnresolvedEntity = UnresolvedEntity Text
    deriving (Show, Typeable)
instance Exception UnresolvedEntity

applyDTD_ :: (MonadBaseControl IO m, MonadIO m, ResourceIO m)
          => DTDCache -> XU.Document -> m X.Document
applyDTD_ dc doc = applyDTD dc doc >>= either (liftIO . throwIO) return

applyDTD :: (MonadBaseControl IO m, MonadIO m, ResourceIO m)
         => DTDCache -> XU.Document -> m (Either UnresolvedEntity X.Document)
applyDTD dc doc@(XU.Document pro@(X.Prologue _ mdoctype _) root epi) = do
    mattrs <-
        case mdoctype of
            Just (X.Doctype _ (Just extid)) -> liftM Just $ loadAttrMap dc extid
            _ ->
                case lookup "{http://www.w3.org/2001/XMLSchema-instance}noNamespaceSchemaLocation" $ XU.elementAttributes root of
                    Just [XU.ContentText uri] -> liftM Just $ loadSchemaAttrMap dc uri
                    _ -> return Nothing
    case mattrs of
        Nothing -> return $ goD (Map.empty, Map.empty) doc
        Just attrs ->
            case go attrs root of
                Left e -> return $ Left e
                Right root' -> return $ Right $ X.Document pro root' epi
  where
    go :: (EntityMap, AttrMap) -> XU.Element -> Either UnresolvedEntity X.Element
    go (ents, attrs) (XU.Element name as ns) = do
        as' <- mapM (resolveAttr ents) as
        ns' <- mapM gon ns
        Right $ X.Element name (as'' as') ns'
      where
        as'' as' =
            case Map.lookup name attrs of
                Nothing -> as'
                Just x -> foldr goa as' $ Map.toList x
        gon (XU.NodeElement e) = fmap X.NodeElement $ go (ents, attrs) e
        gon (XU.NodeComment t) = Right $ X.NodeComment t
        gon (XU.NodeInstruction t) = Right $ X.NodeInstruction t
        gon (XU.NodeContent (XU.ContentText t)) = Right $ X.NodeContent t
        gon (XU.NodeContent (XU.ContentEntity t)) = fmap X.NodeContent $ getEntity ents t

    goa (name, Fixed t) as = (name, t) : filter (\(n, _) -> name /= n) as
    goa (name, Def t) as =
        case lookup name as of
            Nothing -> (name, t) : as
            Just _ -> as

    goD attrs (XU.Document a r b) =
        case go attrs r of
            Left e -> Left e
            Right root' -> Right $ X.Document a root' b

    resolveAttr ents (k, v) = fmap (\ts -> (k, T.concat ts)) $ mapM (resolveAttr' ents) v
    resolveAttr' _ (XU.ContentText t) = Right t
    resolveAttr' ents (XU.ContentEntity t) = getEntity ents t

data Att = Def Text | Fixed Text

type AttrMap = Map.Map X.Name (Map.Map X.Name Att)
type EntityMap = Map.Map Text Text

getEntity :: EntityMap -> Text -> Either UnresolvedEntity Text
getEntity ents t = maybe (Left $ UnresolvedEntity t) Right $ do
    raw <- Map.lookup t ents
    case X.parseText X.def $ TL.fromChunks ["<dummy>", raw, "</dummy>"] of
        Right (X.Document _ (X.Element _ _ nodes) _) -> toContent nodes
        Left{} -> Nothing
  where
    toContent = fmap T.concat . mapM toContent'
    toContent' (X.NodeContent t') = Just t'
    toContent' _ = Nothing
