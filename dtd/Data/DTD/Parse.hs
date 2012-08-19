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
import Data.Conduit hiding (Source, Sink, Conduit)
import qualified Data.Conduit.Internal as CI
import Text.XML.Stream.Parse (detectUtf)
import Data.Conduit.Attoparsec (conduitParser)
import Control.Applicative ((*>), (<*), (<|>), many)
import qualified Data.IORef as I
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Attoparsec.Text as A
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (MonadIO (liftIO))

type ResolveMonad m = ReaderT ResolveReader m

readEID :: (MonadResource m, MonadBaseControl IO m)
        => Catalog
        -> ExternalID
        -> SchemeMap
        -> Pipe l i DTDComponent u m ()
readEID catalog eid sm =
    case resolveURI catalog Nothing eid of
        Nothing -> liftIO $ throwIO $ CannotResolveExternalID eid
        Just uri -> do
            istate <- liftIO $ I.newIORef initState
            let rr = ResolveReader catalog uri istate sm
            readerToEnum rr

readerToEnum :: (MonadBaseControl IO m, MonadThrow m, MonadIO m, MonadResource m)
             => ResolveReader -> Pipe l i DTDComponent u m ()
readerToEnum rr =
    addCatch src0
  where
    src0 =
        readURI (rrSchemeMap rr) (rrBase rr)
                >+> detectUtf
                >+> streamUnresolved
                >+> CL.concatMap id
                >+> resolveEnum rr

    addCatch :: (MonadThrow m, MonadBaseControl IO m)
             => Pipe l i o u m r
             -> Pipe l i o u m r
    addCatch (CI.HaveOutput src close x) = CI.HaveOutput (addCatch src) (addCatch' close) x
    addCatch (CI.NeedInput p c) = CI.NeedInput (addCatch . p) (addCatch . c)
    addCatch (CI.Done r) = CI.Done r
    addCatch (CI.PipeM msrc) = CI.PipeM (addCatch' $ liftM addCatch msrc)
    addCatch (CI.Leftover p i) = CI.Leftover (addCatch p) i

    addCatch' m = m `Lifted.catch` throw rr

throw :: MonadThrow m => ResolveReader -> SomeException -> m a
throw rr e =
    monadThrow $ OccuredAt (show $ toNetworkURI $ rrBase rr) (ResolveOther e)

readFile_ :: FilePath -> IO [DTDComponent]
readFile_ fp = runResourceT $ enumFile fp $$ CL.consume

enumFile :: (MonadBaseControl IO m, MonadResource m) => FilePath -> Pipe l i DTDComponent u m ()
enumFile fp = do
    eid <- lift $ filePathToEID fp
    readEID Map.empty eid $ toSchemeMap [fileScheme]

filePathToEID :: MonadIO m => FilePath -> m ExternalID
filePathToEID = liftM uriToEID . liftIO . decodeString

uriToEID :: URI -> ExternalID
uriToEID = SystemID . T.pack . show . toNetworkURI

streamUnresolved :: MonadThrow m => Pipe l T.Text [U.DTDComponent] r m r
streamUnresolved =
    mapOutput snd $ injectLeftovers $ conduitParser p
  where
    p = (UP.ws >> UP.skipWS >> return []) <|>
        (UP.textDecl >> return []) <|>
        (UP.dtdComponent >>= return . return) <|>
        (A.endOfInput >> return [])

resolveEnum :: (MonadBaseControl IO m, MonadThrow m, MonadIO m, MonadUnsafeIO m)
            => ResolveReader
            -> Pipe l U.DTDComponent DTDComponent r m r
resolveEnum rr = transPipe (evalStateT' rr) $ awaitForever resolvef

evalStateT' :: Monad m
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

resolvef :: (MonadBaseControl IO m, MonadThrow m, MonadIO m, MonadUnsafeIO m)
         => U.DTDComponent
         -> Pipe l U.DTDComponent DTDComponent u (ResolveMonad m) ()

-- passthrough, no modification needed
resolvef (U.DTDNotation x) = yield $ DTDNotation x
resolvef (U.DTDInstruction x) = yield $ DTDInstruction x
resolvef (U.DTDComment x) = yield $ DTDComment x
resolvef (U.DTDEntityDecl (U.ExternalGeneralEntityDecl a b c)) =
    yield $ DTDEntityDecl $ ExternalGeneralEntityDecl a b c

-- look up the EntityValues
resolvef (U.DTDEntityDecl (U.InternalGeneralEntityDecl a b)) = do
    rs <- lift get
    case resolveEntityValue rs b of
        Left e -> lift $ throwError' e
        Right t -> yield $ DTDEntityDecl $ InternalGeneralEntityDecl a t

-- store external entities
resolvef (U.DTDEntityDecl (U.ExternalParameterEntityDecl name eid)) =
    lift $ modify $ \rs -> rs { rsRefEid = insertNoReplace name eid $ rsRefEid rs }

-- store internal entities
resolvef (U.DTDEntityDecl (U.InternalParameterEntityDecl name vals)) = do
    rs <- lift get
    t <- either (lift . throwError') return $ resolveEntityValue rs vals
    lift $ put $ rs { rsRefText = insertNoReplace name t $ rsRefText rs }

-- pull in perefs
resolvef (U.DTDPERef p) = do
    rs <- lift get
    case Map.lookup p $ rsRefEid rs of
        Nothing -> lift $ throwError' $ UnknownPERef p
        Just eid -> do
            rr <- lift ask
            case resolveURI (rrCatalog rr) (Just $ rrBase rr) eid of
                Nothing -> lift $ throwError' $ CannotResolveExternalID eid
                Just uri -> do
                    let rr' = rr { rrBase = uri }
                    x <- lift $ runResourceT $ readerToEnum rr' $$ CL.consume -- FIXME could be more efficient
                    mapM_ yield x

-- element declarations
resolvef (U.DTDElementDecl (U.ElementDecl name' c)) = do
    name <- lift $ either resolvePERefText return name'
    c' <- lift $
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
    yield $ DTDElementDecl $ ElementDecl name c'

-- attribute list
resolvef (U.DTDAttList (U.AttList name' xs)) = do
    name <- lift $ either resolvePERefText return name'
    ys <- lift $ mapM resolveAttDeclPERef xs
    yield $ DTDAttList $ AttList name $ concat ys

-- conditional sections
resolvef (U.DTDCondSecBegin x) = do
    toInclude <-
        case x of
            Left peref -> do
                value <- lift $ resolvePERefText peref
                case value of
                    "INCLUDE" -> return True
                    "IGNORE" -> return False
                    _ -> lift $ throwError' $ InvalidConditionalSectionValue value
            Right y -> return y
    let loop = await >>= maybe
            (lift $ throwError' MissingConditionalSectionEnd)
            go
        go U.DTDCondSecEnd = return ()
        go e = do
            when toInclude $ resolvef e
            loop
    loop

resolvef U.DTDCondSecEnd = lift $ throwError' UnexpectedConditionalSectionEnd

resolveAttDeclPERef :: (MonadIO m, MonadThrow m) => U.AttDeclPERef -> ResolveMonad m [AttDecl]
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

throwError' :: MonadThrow m => ResolveException' -> ResolveMonad m a
throwError' e = do
    uri <- liftM rrBase ask
    lift $ monadThrow $ OccuredAt (show $ toNetworkURI uri) e

runPartial :: A.Result t -> A.Result t
runPartial (A.Partial f) = f ""
runPartial r = r

resolvePERefText :: (MonadIO m, MonadThrow m) => U.PERef -> ResolveMonad m T.Text
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

resolveContentModel :: (MonadIO m, MonadThrow m) => [U.EntityValue] -> ResolveMonad m ContentDecl
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
    | UnexpectedConditionalSectionEnd
    | InvalidConditionalSectionValue T.Text
    | MissingConditionalSectionEnd
  deriving (Show, Typeable)
instance Exception ResolveException'

insertNoReplace :: Ord k => k -> v -> Map.Map k v -> Map.Map k v
insertNoReplace k v m =
    case Map.lookup k m of
        Nothing -> Map.insert k v m
        Just{} -> m
