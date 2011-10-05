{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.DTD.Parse
    ( readFile_
    , enumFile
    , filePathToEID
    , uriToEID
    , readEID
    ) where

import Data.DTD.Types
import Data.XML.Types (ExternalID (SystemID))
import qualified Data.DTD.Types.Unresolved as U
import qualified Data.DTD.Parse.Unresolved as UP
import Control.Exception (Exception, SomeException)
import Data.Enumerator (Enumeratee)
import qualified Data.Enumerator.List as EL
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Error (ErrorT, runErrorT, throwError, Error)
import Data.Enumerator.Unwrap (unwrapEnumeratee)
import Text.XML.Catalog (resolveURI, Catalog)
import Network.URI.Enumerator (URI, SchemeMap, readURI, toNetworkURI, toSchemeMap)
import Network.URI.Enumerator.File (decodeString, fileScheme)
import qualified Data.Enumerator as E
import Text.XML.Stream.Parse (detectUtf)
import Data.Attoparsec.Text.Enumerator (iterParser)
import Control.Applicative ((*>), (<*), (<|>), (<$>))
import qualified Data.IORef as I
import Control.Monad.IO.Control (MonadControlIO)
import qualified Data.Attoparsec.Text as A
import Control.Monad (liftM)

type ResolveMonad m = ErrorT ResolveException (ReaderT ResolveReader m)

readEID :: MonadControlIO m
        => Catalog
        -> ExternalID
        -> (forall m1. MonadControlIO m1 => SchemeMap m1)
        -> E.Enumerator DTDComponent m a
readEID catalog eid sm step = do
    case resolveURI catalog Nothing eid of
        Nothing -> E.throwError $ CannotResolveExternalID eid
        Just uri -> do
            istate <- liftIO $ I.newIORef initState
            let rr = ResolveReader catalog uri istate sm
            readerToEnum rr step

readerToEnum :: MonadControlIO m => ResolveReader -> E.Enumerator DTDComponent m a
readerToEnum rr step =
    (((((readURI (rrSchemeMap rr) (rrBase rr)
                E.$= detectUtf)
                E.$= singleChunk) -- FIXME this is working around an apparent bug in attoparsec-text
                E.$= streamUnresolved)
                E.$= EL.concatMap id)
                E.$= resolveEnum rr) step

-- | For some reason, attoparsec-text seems to be dropping bits of text when
-- being served chunks. This occurred in both streaming and lazy. See
-- lazy-bug.hs for a demonstration.
singleChunk :: Monad m => E.Enumeratee T.Text T.Text m a
singleChunk = E.sequence $ T.concat <$> EL.consume

readFile_ :: FilePath -> IO [DTDComponent]
readFile_ fp = E.run_ $ enumFile fp E.$$ EL.consume

enumFile :: MonadControlIO m => FilePath -> E.Enumerator DTDComponent m a
enumFile fp step = do
    eid <- filePathToEID fp
    readEID Map.empty eid (toSchemeMap [fileScheme]) step

filePathToEID :: MonadIO m => FilePath -> m ExternalID
filePathToEID = liftM uriToEID . liftIO . decodeString

uriToEID :: URI -> ExternalID
uriToEID = SystemID . T.pack . show . toNetworkURI

streamUnresolved :: Monad m => E.Enumeratee T.Text [U.DTDComponent] m a
streamUnresolved =
    E.sequence $ iterParser p
  where
    p = (UP.ws >> UP.skipWS >> return []) <|>
        (UP.textDecl >> return []) <|>
        (UP.dtdComponent >>= return . return)

resolveEnum :: MonadControlIO m
            => ResolveReader
            -> Enumeratee U.DTDComponent DTDComponent m a
resolveEnum rr =
      unwrapEnumeratee (evalStateT' rr)
    $ unwrapEnumeratee runErrorT
    $ EL.concatMapM resolvef

evalStateT' :: MonadIO m
            => ResolveReader
            -> ReaderT ResolveReader m a
            -> m (Either SomeException a)
evalStateT' rr m = do
    a <- runReaderT m rr
    return $ Right a

data ResolveState = ResolveState
    { rsRefText :: Map.Map U.PERef T.Text
    , rsRefEid :: Map.Map U.PERef ExternalID
    }
    deriving Show

data ResolveReader = ResolveReader
    { rrCatalog :: Catalog
    , rrBase :: URI
    , rrState :: I.IORef ResolveState
    , rrSchemeMap :: forall m. MonadControlIO m => SchemeMap m
    }

get :: MonadIO m => ReaderT ResolveReader m ResolveState
get = do
    rr <- ask
    liftIO $ I.readIORef $ rrState rr

put :: MonadIO m => ResolveState -> ReaderT ResolveReader m ()
put rs = do
    rr <- ask
    liftIO $ I.writeIORef (rrState rr) rs

modify :: MonadIO m => (ResolveState -> ResolveState) -> ReaderT ResolveReader m ()
modify f = get >>= put . f

initState :: ResolveState
initState = ResolveState Map.empty Map.empty

resolvef :: MonadControlIO m
         => U.DTDComponent
         -> ResolveMonad m [DTDComponent]

-- passthrough, no modification needed
resolvef (U.DTDNotation x) = return [DTDNotation x]
resolvef (U.DTDInstruction x) = return [DTDInstruction x]
resolvef (U.DTDComment x) = return [DTDComment x]
resolvef (U.DTDEntityDecl (U.ExternalGeneralEntityDecl a b c)) =
    return [DTDEntityDecl $ ExternalGeneralEntityDecl a b c]

-- look up the EntityValues
resolvef (U.DTDEntityDecl (U.InternalGeneralEntityDecl a b)) = do
    rs <- lift get
    case resolveEntityValue rs b of
        Left e -> throwError e
        Right t -> return [DTDEntityDecl $ InternalGeneralEntityDecl a t]

-- store external entities
resolvef (U.DTDEntityDecl (U.ExternalParameterEntityDecl name eid)) = do
    lift $ modify $ \rs -> rs { rsRefEid = Map.insert name eid $ rsRefEid rs }
    return []

-- store internal entities
resolvef (U.DTDEntityDecl (U.InternalParameterEntityDecl name vals)) = do
    rs <- lift get
    t <- either throwError return $ resolveEntityValue rs vals
    lift $ put $ rs { rsRefText = Map.insert name t $ rsRefText rs }
    return []

-- pull in perefs
resolvef (U.DTDPERef p) = do
    rs <- lift get
    case Map.lookup p $ rsRefEid rs of
        Nothing -> throwError $ UnknownPERef p
        Just eid -> do
            rr <- lift ask
            case resolveURI (rrCatalog rr) (Just $ rrBase rr) eid of
                Nothing -> throwError $ CannotResolveExternalID eid
                Just uri -> do
                    let rr' = rr { rrBase = uri }
                    E.run_ $ readerToEnum rr' E.$$ EL.consume

-- element declarations
resolvef (U.DTDElementDecl (U.ElementDecl name' c)) = do
    name <- either resolvePERefText return name'
    c' <-
        case c of
            U.ContentEmpty -> return ContentEmpty
            U.ContentAny -> return ContentAny
            U.ContentElement cm -> return $ ContentElement cm
            U.ContentMixed cm -> return $ ContentMixed cm
            U.ContentPERef p -> do
                t <- resolvePERefText p
                case runPartial $ A.parse (UP.skipWS *> UP.contentDecl <* UP.skipWS) t of
                    A.Done "" x ->
                        case x of
                            U.ContentPERef{} -> throwError $ RecursiveContentDeclPERef p
                            U.ContentEmpty -> return ContentEmpty
                            U.ContentAny -> return ContentAny
                            U.ContentElement cm -> return $ ContentElement cm
                            U.ContentMixed cm -> return $ ContentMixed cm
                    x -> throwError $ InvalidContentDecl p t x
    return [DTDElementDecl $ ElementDecl name c']

-- attribute list
resolvef (U.DTDAttList (U.AttList name' xs)) = do
    name <- either resolvePERefText return name'
    ys <- mapM resolveAttDeclPERef xs
    return [DTDAttList $ AttList name $ concat ys]

resolveAttDeclPERef :: MonadControlIO m => U.AttDeclPERef -> ResolveMonad m [AttDecl]
resolveAttDeclPERef (U.ADPDecl d) = return [d]
resolveAttDeclPERef (U.ADPPERef p) = do
    t <- resolvePERefText p
    case runPartial $ A.parse (A.many1 UP.attDecl) t of
        A.Done "" x -> return x
        x -> throwError $ InvalidAttDecl p t x

runPartial :: A.Result t -> A.Result t
runPartial (A.Partial f) = f ""
runPartial r = r

resolvePERefText :: MonadIO m => U.PERef -> ResolveMonad m T.Text
resolvePERefText p = do
    rs <- lift get
    maybe (throwError $ UnknownPERefText p) return $ Map.lookup p $ rsRefText rs

resolveEntityValue :: ResolveState -> [U.EntityValue] -> Either ResolveException T.Text
resolveEntityValue rs evs =
    fmap T.concat $ mapM go evs
  where
    go (U.EntityText t) = Right t
    go (U.EntityPERef p) =
        case Map.lookup p $ rsRefText rs of
            Nothing -> Left $ UnknownPERefValue p
            Just t -> Right t

data ResolveException
    = UnknownPERef U.PERef
    | UnknownPERefValue U.PERef
    | UnknownPERefText U.PERef
    | CannotResolveExternalID ExternalID
    | InvalidContentDecl U.PERef T.Text (A.Result U.ContentDecl)
    | InvalidAttDecl U.PERef T.Text (A.Result [U.AttDecl])
    | RecursiveContentDeclPERef U.PERef
  deriving (Show, Typeable)
instance Exception ResolveException
instance Error ResolveException
