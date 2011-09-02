{-# LANGUAGE DeriveDataTypeable #-}
-- | DOM-based XML parsing and rendering.
--
-- In this module, attribute values and content nodes can contain either raw
-- text or entities. In most cases, these can be fully resolved at parsing. If
-- that is the case for your documents, the "Text.XML.Enumerator.Resolved"
-- module provides simplified datatypes that only contain raw text.
module Text.XML.Enumerator.Document
    ( -- * Non-streaming functions
      writeFile
    , writePrettyFile
    , readFile
    , readFile_
      -- * Lazy bytestrings
    , renderLBS
    , prettyLBS
    , parseLBS
    , parseLBS_
      -- * Streaming functions
    , toEvents
    , fromEvents
    , renderBuilder
    , renderBytes
    , renderText
      -- ** Pretty versions
    , prettyBuilder
    , prettyBytes
    , prettyText
      -- * Exceptions
    , InvalidEventStream (InvalidEventStream)
      -- * Internal
    , lazyConsume
    ) where

import Prelude hiding (writeFile, readFile)
import Data.XML.Types
import Data.Enumerator
    ( ($$), enumList, joinE, Enumerator, Iteratee, peek, returnI
    , throwError, joinI, run, run_, Step (Continue), Stream (Chunks)
    )
import Control.Exception (Exception, SomeException)
import Data.Typeable (Typeable)
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder (Builder)
import Control.Monad.IO.Class (MonadIO)
import qualified Text.XML.Enumerator.Render as R
import qualified Text.XML.Enumerator.Parse as P
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import qualified System.IO as SIO
import Data.Enumerator.Binary (enumFile, iterHandle)
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.MVar as M
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Data.Functor.Identity (runIdentity)

readFile :: FilePath -> P.DecodeEntities -> IO (Either SomeException Document)
readFile fn de = run $ enumFile fn $$ joinI $ P.parseBytes de $$ fromEvents

readFile_ :: FilePath -> P.DecodeEntities -> IO Document
readFile_ fn de = run_ $ enumFile fn $$ joinI $ P.parseBytes de $$ fromEvents

writeFile :: FilePath -> Document -> IO ()
writeFile fn doc = SIO.withBinaryFile fn SIO.WriteMode $ \h ->
    run_ $ renderBytes doc $$ iterHandle h

-- | Pretty prints via 'prettyBytes'.
writePrettyFile :: FilePath -> Document -> IO ()
writePrettyFile fn doc = SIO.withBinaryFile fn SIO.WriteMode $ \h ->
    run_ $ prettyBytes doc $$ iterHandle h

renderLBS :: Document -> L.ByteString
renderLBS doc =
    L.fromChunks $ unsafePerformIO $ lazyConsume $ renderBytes doc

-- | Pretty prints via 'prettyBytes'.
prettyLBS :: Document -> L.ByteString
prettyLBS doc =
    L.fromChunks $ unsafePerformIO $ lazyConsume $ prettyBytes doc

parseLBS :: L.ByteString -> P.DecodeEntities -> Either SomeException Document
parseLBS lbs de = runIdentity
                $ run $ enumSingle (L.toChunks lbs)
                     $$ joinI $ P.parseBytes de $$ fromEvents

parseLBS_ :: L.ByteString -> P.DecodeEntities -> Document
parseLBS_ lbs de = runIdentity
                 $ run_ $ enumSingle (L.toChunks lbs)
                       $$ joinI $ P.parseBytes de $$ fromEvents

enumSingle :: Monad m => [a] -> Enumerator a m b
enumSingle as (Continue k) = k $ Chunks as
enumSingle _ step = returnI step

lazyConsume :: Enumerator a IO () -> IO [a]
lazyConsume enum = do
    toGrabber <- M.newEmptyMVar
    toFiller <- M.newMVar True
    _ <- forkIO $ run_ $ enum $$ filler toGrabber toFiller
    grabber toGrabber toFiller
  where
    grabber toGrabber toFiller = do
        x <- M.takeMVar toGrabber
        case x of
            Nothing -> return []
            Just x' -> do
                M.putMVar toFiller True
                xs <- unsafeInterleaveIO $ grabber toGrabber toFiller
                return $ x' : xs
    filler toGrabber toFiller = do
        cont <- liftIO $ M.takeMVar toFiller
        if cont
            then do
                x <- EL.head
                liftIO $ M.putMVar toGrabber x
                case x of
                    Nothing -> return ()
                    Just _ -> filler toGrabber toFiller
            else liftIO $ M.putMVar toGrabber Nothing

data InvalidEventStream = InvalidEventStream String
    deriving (Show, Typeable)
instance Exception InvalidEventStream

prettyBuilder :: MonadIO m => Document -> Enumerator Builder m a
prettyBuilder doc = enumList 8 (toEvents doc) `joinE` R.prettyBuilder

prettyBytes :: MonadIO m => Document -> Enumerator ByteString m a
prettyBytes doc = enumList 8 (toEvents doc) `joinE` R.prettyBytes

prettyText :: MonadIO m => Document -> Enumerator Text m a
prettyText doc = enumList 8 (toEvents doc) `joinE` R.prettyText

renderBuilder :: MonadIO m => Document -> Enumerator Builder m a
renderBuilder doc = enumList 8 (toEvents doc) `joinE` R.renderBuilder

renderBytes :: MonadIO m => Document -> Enumerator ByteString m a
renderBytes doc = enumList 8 (toEvents doc) `joinE` R.renderBytes

renderText :: MonadIO m => Document -> Enumerator Text m a
renderText doc = enumList 8 (toEvents doc) `joinE` R.renderText

fromEvents :: Monad m => Iteratee Event m Document
fromEvents = do
    skip EventBeginDocument
    d <- Document <$> goP <*> require goE <*> goM
    skip EventEndDocument
    y <- EL.head
    if y == Nothing
        then return d
        else throwError $ InvalidEventStream $ "Trailing matter after epilogue: " ++ show y
  where
    skip e = do
        x <- peek
        if x == Just e then EL.drop 1 else return ()
    many f =
        go id
      where
        go front = do
            x <- f
            case x of
                Nothing -> return $ front []
                Just y -> go (front . (:) y)
    dropReturn x = EL.drop 1 >> return x
    require f = do
        x <- f
        case x of
            Just y -> return y
            Nothing -> do
                y <- EL.head
                throwError $ InvalidEventStream $ "Document must have a single root element, got: " ++ show y
    goP = Prologue <$> goM <*> goD <*> goM
    goM = many goM'
    goM' = do
        x <- peek
        case x of
            Just (EventInstruction i) -> dropReturn $ Just $ MiscInstruction i
            Just (EventComment t) -> dropReturn $ Just $ MiscComment t
            Just (EventContent (ContentText t))
                | T.all isSpace t -> EL.drop 1 >> goM'
            _ -> return Nothing
    goD = do
        x <- peek
        case x of
            Just (EventBeginDoctype name meid) -> do
                EL.drop 1
                dropTillDoctype
                return (Just $ Doctype name meid)
            _ -> return Nothing
    dropTillDoctype = do
        x <- EL.head
        case x of
            -- Leaving the following line commented so that the intention of
            -- this function stays clear. I figure in the future xml-types will
            -- be expanded again to support some form of EventDeclaration
            --
            -- Just (EventDeclaration _) -> dropTillDoctype
            Just EventEndDoctype -> return ()
            _ -> throwError $ InvalidEventStream $ "Invalid event during doctype, got: " ++ show x
    goE = do
        x <- peek
        case x of
            Just (EventBeginElement n as) -> Just <$> goE' n as
            _ -> return Nothing
    goE' n as = do
        EL.drop 1
        ns <- many goN
        y <- EL.head
        if y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else throwError $ InvalidEventStream $ "Missing end element for " ++ show n ++ ", got: " ++ show y
    goN = do
        x <- peek
        case x of
            Just (EventBeginElement n as) -> (Just . NodeElement) <$> goE' n as
            Just (EventInstruction i) -> dropReturn $ Just $ NodeInstruction i
            Just (EventContent c) -> dropReturn $ Just $ NodeContent c
            Just (EventComment t) -> dropReturn $ Just $ NodeComment t
            Just (EventCDATA t) -> dropReturn $ Just $ NodeContent $ ContentText t
            _ -> return Nothing

toEvents :: Document -> [Event]
toEvents (Document prol root epi) =
      (EventBeginDocument :)
    . goP prol . goE root . goM epi $ [EventEndDocument]
  where
    goP (Prologue before doctype after) =
        goM before . maybe id goD doctype . goM after
    goM [] = id
    goM [x] = (goM' x :)
    goM (x:xs) = (goM' x :) . goM xs
    goM' (MiscInstruction i) = EventInstruction i
    goM' (MiscComment t) = EventComment t
    goD (Doctype name meid) =
        (:) (EventBeginDoctype name meid)
      . (:) EventEndDoctype
    goE (Element name as ns) =
          (EventBeginElement name as :)
        . goN ns
        . (EventEndElement name :)
    goN [] = id
    goN [x] = goN' x
    goN (x:xs) = goN' x . goN xs
    goN' (NodeElement e) = goE e
    goN' (NodeInstruction i) = (EventInstruction i :)
    goN' (NodeContent c) = (EventContent c :)
    goN' (NodeComment t) = (EventComment t :)

compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
    compressNodes $ NodeContent (ContentText $ x `T.append` y) : z
compressNodes (x:xs) = x : compressNodes xs
