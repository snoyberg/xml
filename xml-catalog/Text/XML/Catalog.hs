{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Used for loading a catalog file, caching DTDs and applying DTDs to
-- documents.
module Text.XML.Catalog
    ( -- * Catalogs
      Catalog
    , PubSys (..)
    , loadCatalog
      -- * Resolving
    , resolveURI
    ) where

import Prelude hiding (FilePath)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Text.XML as X
import Control.Monad (foldM)
import Network.URI.Conduit
import qualified Data.Text as T
import qualified Data.Conduit as C

-- | Either a public or system identifier.
data PubSys = Public Text | System Text
    deriving (Eq, Show, Ord)

-- | An XML catalog, mapping public and system identifiers to filepaths.
type Catalog = Map.Map PubSys URI

-- | Load a 'Catalog' from the given path.
loadCatalog :: C.MonadBaseControl IO m => SchemeMap -> URI -> m Catalog
loadCatalog sm uri = do
    X.Document _ (X.Element _ _ ns) _ <- C.liftBase $ C.runResourceT $
        readURI sm uri C.<$$> X.sinkDoc X.def
    foldM (addNode Nothing) Map.empty ns
  where
    addNode mbase0 c (X.NodeElement (X.Element name as ns)) = do
        c'' <- c'
        foldM (addNode mbase) c'' ns
      where
        mbase = maybe mbase0 Just $ lookup "{http://www.w3.org/XML/1998/namespace}base" as
        withBase = maybe id T.append mbase

        c' =
            case name of
                "{urn:oasis:names:tc:entity:xmlns:xml:catalog}public" ->
                    case (lookup "publicId" as, lookup "uri" as) of
                        (Just pid, Just ref) ->
                            case parseURIReference (withBase ref) >>= flip relativeTo uri of
                                Just uri' -> return $ Map.insert (Public pid) uri' c
                                Nothing -> return c
                        _ -> return c
                "{urn:oasis:names:tc:entity:xmlns:xml:catalog}system" ->
                    case (lookup "systemId" as, lookup "uri" as) of
                        (Just sid, Just ref) ->
                            case parseURIReference (withBase ref) >>= flip relativeTo uri of
                                Just uri' -> return $ Map.insert (System sid) uri' c
                                Nothing -> return c
                        _ -> return c
                "{urn:oasis:names:tc:entity:xmlns:xml:catalog}nextCatalog" ->
                    case lookup "catalog" as of
                        Just catalog ->
                            case parseURIReference catalog >>= flip relativeTo uri of
                                Just uri' -> do
                                    c'' <- loadCatalog sm uri'
                                    return $ c'' `Map.union` c
                                Nothing -> return c
                        Nothing -> return c
                _ -> return c
    addNode _ c _ = return c

resolveURI :: Catalog
           -> Maybe URI -- ^ base URI for relative system identifiers
           -> X.ExternalID
           -> Maybe URI
resolveURI catalog mbase (X.PublicID public system) =
    case Map.lookup (Public public) catalog of
        Nothing -> resolveURI catalog mbase (X.SystemID system)
        Just x -> Just x
resolveURI catalog mbase (X.SystemID system) =
    case Map.lookup (System system) catalog of
        Nothing ->
            case parseURI system of
                Just uri -> Just uri
                Nothing -> do
                    base <- mbase
                    ref <- parseURIReference system
                    ref `relativeTo` base
        Just x -> Just x
