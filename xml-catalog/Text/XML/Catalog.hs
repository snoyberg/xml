{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Used for loading a catalog file, caching DTDs and applying DTDs to
-- documents.
module Text.XML.Catalog
    ( -- * Catalogs
      Catalog
    , PubSys (..)
    , loadCatalog
      -- * DTD caching
    , DTDCache
    , newDTDCache
    , loadDTD
    , UnknownExternalID (..)
      -- * Applying DTDs
    , applyDTD
    ) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, encodeString, directory, (</>), fromText)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Text.XML.Enumerator.Resolved as X
import Control.Monad (foldM)
import qualified Data.IORef as I
import qualified Data.Map as Map
import Data.XML.DTD.Parse (parseDTD)
import qualified Data.XML.DTD.Types as D
import Data.Text (Text)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Text.Lazy.IO as TLIO
import Data.Maybe (mapMaybe)
import qualified Data.HashTable.IO as HT
import Data.Hashable (Hashable (..), combine)

-- | Either a public or system identifier.
data PubSys = Public Text | System Text
    deriving (Eq, Show, Ord)

instance Hashable PubSys where
    hash (Public a)  = 0 `hashWithSalt` a
    hash (System b) = 1 `hashWithSalt` b
    hashWithSalt s (Public a)  = s `combine` 0 `hashWithSalt` a
    hashWithSalt s (System b) = s `combine` 1 `hashWithSalt` b

-- | An XML catalog, mapping public and system identifiers to filepaths.
type Catalog = Map.Map PubSys FilePath

-- | Load a 'Catalog' from the given path.
loadCatalog :: FilePath -> IO Catalog
loadCatalog fp = do
    X.Document _ (X.Element _ _ ns) _ <- X.readFile_ (encodeString fp) X.decodeEntities
    foldM addNode Map.empty ns
  where
    dir = directory fp
    addNode c (X.NodeElement (X.Element name as ns)) = do
        foldM addNode c' ns
      where
        c' =
            case name of
                "{urn:oasis:names:tc:entity:xmlns:xml:catalog}public" ->
                    case (lookup "publicId" as, lookup "uri" as) of
                        (Just pid, Just uri) -> Map.insert (Public pid) (dir </> fromText uri) c
                        _ -> c
                "{urn:oasis:names:tc:entity:xmlns:xml:catalog}system" ->
                    case (lookup "systemId" as, lookup "uri" as) of
                        (Just sid, Just uri) -> Map.insert (System sid) (dir </> fromText uri) c
                        _ -> c
                _ -> c
    addNode c _ = return c

data DTDCache = DTDCache
    { _dcCache :: HT.BasicHashTable PubSys D.DTD
    , _dcCatalog :: Catalog
    }

newDTDCache :: Catalog -> IO DTDCache
newDTDCache c = do
    x <- HT.new
    return $ DTDCache x c

loadDTD :: DTDCache -> X.ExternalID -> IO D.DTD
loadDTD (DTDCache icache catalog) ext = do
    res <- HT.lookup icache pubsys
    case res of
        Just dtd -> return dtd
        Nothing ->
            case Map.lookup pubsys catalog of
                Nothing -> throwIO $ UnknownExternalID ext
                Just fp -> do
                    dtd <- fmap parseDTD $ TLIO.readFile $ encodeString fp
                    HT.insert icache pubsys dtd
                    return dtd
  where
    pubsys =
        case ext of
            X.SystemID t -> System t
            X.PublicID t _ -> Public t

data UnknownExternalID = UnknownExternalID X.ExternalID
    deriving (Show, Typeable)
instance Exception UnknownExternalID

applyDTD :: DTDCache -> X.Document -> IO X.Document
applyDTD dc doc@(X.Document pro@(X.Prologue _ mdoctype _) root epi) =
    case mdoctype of
        Just (X.Doctype _ (Just extid)) -> do
            dtd <- loadDTD dc extid
            let attrs = toAttrs dtd
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

toAttrs :: D.DTD -> Map.Map X.Name [(X.Name, Att)]
toAttrs (D.DTD _ comps) =
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
