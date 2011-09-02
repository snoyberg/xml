{-# LANGUAGE DeriveDataTypeable #-}
-- | DOM-based XML parsing and rendering.
--
-- In this module, attribute values and content nodes can contain either raw
-- text or entities. In most cases, these can be fully resolved at parsing. If
-- that is the case for your documents, the "Text.XML" module provides
-- simplified datatypes that only contain raw text.
module Text.XML.Unresolved
    ( -- * Non-streaming functions
      writeFile
    , readFile
    , readFile_
      -- * Lazy bytestrings
    , renderLBS
    , parseLBS
    , parseLBS_
      -- * Streaming functions
    , toEvents
    , fromEvents
    , renderBuilder
    , renderBytes
    , renderText
      -- * Exceptions
    , InvalidEventStream (InvalidEventStream)
      -- * Internal
    , lazyConsume
      -- * Settings
    , P.def
      -- ** Parse
    , P.ParseSettings
    , P.psDecodeEntities
      -- ** Render
    , R.RenderSettings
    , R.rsPretty
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Text.XML.Stream.Render as R
import qualified Text.XML.Stream.Parse as P
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Control.Monad       (when)
import qualified System.IO as SIO
import Data.Enumerator.Binary (enumFile, iterHandle)
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as L
import qualified Control.Concurrent.MVar as M
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Control.Concurrent (forkIO)
import Data.Functor.Identity (runIdentity)

readFile :: P.ParseSettings -> FilePath -> IO (Either SomeException Document)
readFile de fn = run $ enumFile fn $$ joinI $ P.parseBytes de $$ fromEvents

readFile_ :: P.ParseSettings -> FilePath -> IO Document
readFile_ de fn = run_ $ enumFile fn $$ joinI $ P.parseBytes de $$ fromEvents

writeFile :: R.RenderSettings -> FilePath -> Document -> IO ()
writeFile rs fn doc = SIO.withBinaryFile fn SIO.WriteMode $ \h ->
    run_ $ renderBytes rs doc $$ iterHandle h

renderLBS :: R.RenderSettings -> Document -> L.ByteString
renderLBS rs doc =
    L.fromChunks $ unsafePerformIO $ lazyConsume $ renderBytes rs doc

parseLBS :: P.ParseSettings -> L.ByteString -> Either SomeException Document
parseLBS de lbs = runIdentity
                $ run $ enumSingle (L.toChunks lbs)
                     $$ joinI $ P.parseBytes de $$ fromEvents

parseLBS_ :: P.ParseSettings -> L.ByteString -> Document
parseLBS_ de lbs = runIdentity
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

renderBuilder :: MonadIO m => R.RenderSettings -> Document -> Enumerator Builder m a
renderBuilder rs doc = enumList 8 (toEvents doc) `joinE` R.renderBuilder rs

renderBytes :: MonadIO m => R.RenderSettings -> Document -> Enumerator ByteString m a
renderBytes rs doc = enumList 8 (toEvents doc) `joinE` R.renderBytes rs

renderText :: MonadIO m => R.RenderSettings -> Document -> Enumerator Text m a
renderText rs doc = enumList 8 (toEvents doc) `joinE` R.renderText rs

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
        when (x == Just e) (EL.drop 1)
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
