{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Exception (Exception, SomeException, throwIO)
import qualified Control.Exception.Lifted as Lifted
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Text.XML.Catalog (resolveURI, Catalog)
import Network.URI.Conduit (URI, SchemeMap, readURI, toNetworkURI, toSchemeMap)
import Network.URI.Conduit.File (decodeString, fileScheme)
import qualified Data.Conduit as C
import Text.XML.Stream.Parse (detectUtf)
import Data.Conduit.Attoparsec (sinkParser)
import Control.Applicative ((*>), (<*), (<|>), (<$>), many)
import qualified Data.IORef as I
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Attoparsec.Text as A
import Control.Monad (liftM)
import Control.Monad.Trans.Resource (ResourceIO)
import Control.Monad.IO.Class (liftIO)

type ResolveMonad m = ReaderT ResolveReader m

readEID :: (ResourceIO m, MonadBaseControl IO m)
        => Catalog
        -> ExternalID
        -> SchemeMap
        -> C.Source m DTDComponent
readEID catalog eid sm = C.Source $ do
    case resolveURI catalog Nothing eid of
        Nothing -> C.liftBase $ throwIO $ CannotResolveExternalID eid
        Just uri -> do
            istate <- C.liftBase $ I.newIORef initState
            let rr = ResolveReader catalog uri istate sm
            C.prepareSource $ readerToEnum rr

readerToEnum :: (MonadBaseControl IO m, ResourceIO m)
             => ResolveReader -> C.Source m DTDComponent
readerToEnum rr = C.Source $ do
    C.PreparedSource pull close <- C.prepareSource $
        readURI (rrSchemeMap rr) (rrBase rr)
                C.$= detectUtf
                C.$= singleChunk -- FIXME this is working around an apparent bug in attoparsec-text
                C.$= streamUnresolved
                C.$= CL.concatMap id
                C.$= resolveEnum rr
    return $ C.PreparedSource (pull `Lifted.catch` (lift . throw rr)) close


throw :: C.ResourceThrow m => ResolveReader -> SomeException -> m a
throw rr e =
    C.resourceThrow $ OccuredAt (show $ toNetworkURI $ rrBase rr) (ResolveOther e)

-- | For some reason, attoparsec-text seems to be dropping bits of text when
-- being served chunks. This occurred in both streaming and lazy. See
-- lazy-bug.hs for a demonstration.
singleChunk :: C.Resource m => C.Conduit T.Text m T.Text
singleChunk = C.sequence $ T.concat <$> CL.consume

readFile_ :: FilePath -> IO [DTDComponent]
readFile_ fp = C.runResourceT $ enumFile fp C.$$ CL.consume

enumFile :: (MonadBaseControl IO m, ResourceIO m) => FilePath -> C.Source m DTDComponent
enumFile fp = C.Source $ do
    eid <- lift $ filePathToEID fp
    C.prepareSource $ readEID Map.empty eid $ toSchemeMap [fileScheme]

filePathToEID :: ResourceIO m => FilePath -> m ExternalID
filePathToEID = liftM uriToEID . liftIO . decodeString

uriToEID :: URI -> ExternalID
uriToEID = SystemID . T.pack . show . toNetworkURI

streamUnresolved :: ResourceIO m => C.Conduit T.Text m [U.DTDComponent]
streamUnresolved =
    C.sequence $ sinkParser p
  where
    p = (UP.ws >> UP.skipWS >> return []) <|>
        (UP.textDecl >> return []) <|>
        (UP.dtdComponent >>= return . return)

resolveEnum :: (ResourceIO m, MonadBaseControl IO m)
            => ResolveReader
            -> C.Conduit U.DTDComponent m DTDComponent
resolveEnum rr =
      C.transConduit (evalStateT' rr)
    $ CL.concatMapM resolvef

evalStateT' :: ResourceIO m
            => ResolveReader
            -> ReaderT ResolveReader m a
            -> m a
evalStateT' rr m = do
    a <- runReaderT m rr
    return a

data ResolveState = ResolveState
    { rsRefText :: Map.Map U.PERef T.Text
    , rsRefEid :: Map.Map U.PERef ExternalID
    }
    deriving Show

data ResolveReader = ResolveReader
    { rrCatalog :: Catalog
    , rrBase :: URI
    , rrState :: I.IORef ResolveState
    , rrSchemeMap :: SchemeMap
    }

get :: ResourceIO m => ReaderT ResolveReader m ResolveState
get = do
    rr <- ask
    liftIO $ I.readIORef $ rrState rr

put :: ResourceIO m => ResolveState -> ReaderT ResolveReader m ()
put rs = do
    rr <- ask
    liftIO $ I.writeIORef (rrState rr) rs

modify :: ResourceIO m => (ResolveState -> ResolveState) -> ReaderT ResolveReader m ()
modify f = get >>= put . f

initState :: ResolveState
initState = ResolveState Map.empty Map.empty

resolvef :: (MonadBaseControl IO m, ResourceIO m)
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
    rs <- get
    case resolveEntityValue rs b of
        Left e -> throwError' e
        Right t -> return [DTDEntityDecl $ InternalGeneralEntityDecl a t]

-- store external entities
resolvef (U.DTDEntityDecl (U.ExternalParameterEntityDecl name eid)) = do
    modify $ \rs -> rs { rsRefEid = insertNoReplace name eid $ rsRefEid rs }
    return []

-- store internal entities
resolvef (U.DTDEntityDecl (U.InternalParameterEntityDecl name vals)) = do
    rs <- get
    t <- either throwError' return $ resolveEntityValue rs vals
    put $ rs { rsRefText = insertNoReplace name t $ rsRefText rs }
    return []

-- pull in perefs
resolvef (U.DTDPERef p) = do
    rs <- get
    case Map.lookup p $ rsRefEid rs of
        Nothing -> throwError' $ UnknownPERef p
        Just eid -> do
            rr <- ask
            case resolveURI (rrCatalog rr) (Just $ rrBase rr) eid of
                Nothing -> throwError' $ CannotResolveExternalID eid
                Just uri -> do
                    let rr' = rr { rrBase = uri }
                    C.runResourceT $ readerToEnum rr' C.$$ CL.consume

-- element declarations
resolvef (U.DTDElementDecl (U.ElementDecl name' c)) = do
    name <- either resolvePERefText return name'
    c' <-
        case c of
            U.ContentEmpty -> return ContentEmpty
            U.ContentAny -> return ContentAny
            U.ContentElement ev -> resolveContentModel ev
            U.ContentMixed cm -> return $ ContentMixed cm
            U.ContentPERef p -> do
                t <- resolvePERefText p
                case runPartial $ A.parse (UP.skipWS *> UP.contentDecl <* UP.skipWS) t of
                    A.Done "" x ->
                        case x of
                            U.ContentPERef{} -> throwError' $ RecursiveContentDeclPERef p
                            U.ContentEmpty -> return ContentEmpty
                            U.ContentAny -> return ContentAny
                            U.ContentElement cm -> resolveContentModel cm
                            U.ContentMixed cm -> return $ ContentMixed cm
                    x -> throwError' $ InvalidContentDecl p t x
    return [DTDElementDecl $ ElementDecl name c']

-- attribute list
resolvef (U.DTDAttList (U.AttList name' xs)) = do
    name <- either resolvePERefText return name'
    ys <- mapM resolveAttDeclPERef xs
    return [DTDAttList $ AttList name $ concat ys]

resolveAttDeclPERef :: ResourceIO m => U.AttDeclPERef -> ResolveMonad m [AttDecl]
resolveAttDeclPERef (U.ADPDecl (U.AttDecl name typ def)) = do
    typ' <-
        case typ of
            U.ATPType t -> return t
            U.ATPPERef p -> do
                t <- resolvePERefText p
                case runPartial $ A.parse UP.attType $ T.strip t `T.append` " " of
                    A.Done "" x -> return x
                    x -> throwError' $ InvalidAttType p t x
    return [AttDecl name typ' def]
resolveAttDeclPERef (U.ADPPERef p) = do
    t <- resolvePERefText p
    case runPartial $ A.parse (many UP.attDecl) $ T.strip t of
        A.Done "" x -> liftM concat $ mapM (resolveAttDeclPERef . U.ADPDecl) x
        x -> throwError' $ InvalidAttDecl p t x

throwError' :: ResourceIO m => ResolveException' -> ResolveMonad m a
throwError' e = do
    uri <- liftM rrBase ask
    lift $ C.resourceThrow $ OccuredAt (show $ toNetworkURI uri) e

runPartial :: A.Result t -> A.Result t
runPartial (A.Partial f) = f ""
runPartial r = r

resolvePERefText :: ResourceIO m => U.PERef -> ResolveMonad m T.Text
resolvePERefText p = do
    rs <- get
    maybe (throwError' $ UnknownPERefText p) return $ Map.lookup p $ rsRefText rs

resolveEntityValue :: ResolveState -> [U.EntityValue] -> Either ResolveException' T.Text
resolveEntityValue rs evs =
    fmap T.concat $ mapM go evs
  where
    go (U.EntityText t) = Right t
    go (U.EntityPERef p) =
        case Map.lookup p $ rsRefText rs of
            Nothing -> Left $ UnknownPERefValue p
            Just t -> Right t

resolveContentModel :: ResourceIO m => [U.EntityValue] -> ResolveMonad m ContentDecl
resolveContentModel ev = do
    rs <- get
    text <- either throwError' return $ resolveEntityValue rs ev
    case runPartial $ A.parse UP.contentModel $ T.strip text of
        A.Done "" x -> return $ ContentElement x
        x -> throwError' $ InvalidContentModel text x

data ResolveException = OccuredAt String ResolveException'
  deriving (Show, Typeable)
instance Exception ResolveException

data ResolveException'
    = UnknownPERef U.PERef
    | UnknownPERefValue U.PERef
    | UnknownPERefText U.PERef
    | CannotResolveExternalID ExternalID
    | InvalidContentDecl U.PERef T.Text (A.Result U.ContentDecl)
    | InvalidContentModel T.Text (A.Result U.ContentModel)
    | InvalidAttDecl U.PERef T.Text (A.Result [U.AttDecl])
    | InvalidAttType U.PERef T.Text (A.Result U.AttType)
    | RecursiveContentDeclPERef U.PERef
    | ResolveOther SomeException
  deriving (Show, Typeable)
instance Exception ResolveException'

insertNoReplace :: Ord k => k -> v -> Map.Map k v -> Map.Map k v
insertNoReplace k v m =
    case Map.lookup k m of
        Nothing -> Map.insert k v m
        Just{} -> m
