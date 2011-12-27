{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
      -- * Lazy bytestrings
    , renderLBS
    , parseLBS
    , parseLBS_
      -- * Byte streams
    , sinkDoc
      -- * Streaming functions
    , toEvents
    , fromEvents
    , renderBuilder
    , renderBytes
    , renderText
      -- * Exceptions
    , InvalidEventStream (InvalidEventStream)
      -- * Settings
    , P.def
      -- ** Parse
    , P.ParseSettings
    , P.psDecodeEntities
      -- ** Render
    , R.RenderSettings
    , R.rsPretty
    ) where

import Prelude hiding (writeFile, readFile, FilePath)
import Filesystem.Path.CurrentOS (FilePath, encodeString)
import Data.XML.Types
import Control.Exception (Exception, SomeException)
import Data.Typeable (Typeable)
import Blaze.ByteString.Builder (Builder)
import qualified Text.XML.Stream.Render as R
import qualified Text.XML.Stream.Parse as P
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Control.Monad       (when)
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Lazy (lazyConsume)
import Control.Exception (throw)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceUnsafeIO, runExceptionT)
import Control.Monad.ST (runST)

readFile :: P.ParseSettings -> FilePath -> IO Document
readFile ps fp = C.runResourceT $ P.parseFile ps fp C.$$ fromEvents

sinkDoc :: C.ResourceThrow m
        => P.ParseSettings -> C.Sink ByteString m Document
sinkDoc ps = P.parseBytes ps C.=$ fromEvents

writeFile :: R.RenderSettings -> FilePath -> Document -> IO ()
writeFile rs fp doc =
    C.runResourceT $ renderBytes rs doc C.$$ CB.sinkFile (encodeString fp)

renderLBS :: R.RenderSettings -> Document -> L.ByteString
renderLBS rs doc =
    L.fromChunks $ unsafePerformIO
                 -- not generally safe, but we know that runResourceT
                 -- will not deallocate any of the resources being used
                 -- by the process
                 $ C.runResourceT
                 $ lazyConsume
                 $ renderBytes rs doc

parseLBS :: P.ParseSettings -> L.ByteString -> Either SomeException Document
parseLBS ps lbs =
    runST $ runExceptionT
          $ C.runResourceT
          $ CL.sourceList (L.toChunks lbs) C.$$ sinkDoc ps

parseLBS_ :: P.ParseSettings -> L.ByteString -> Document
parseLBS_ ps lbs = either throw id $ parseLBS ps lbs

data InvalidEventStream = InvalidEventStream String
    deriving (Show, Typeable)
instance Exception InvalidEventStream

renderBuilder :: C.Resource m => R.RenderSettings -> Document -> C.Source m Builder
renderBuilder rs doc = CL.sourceList (toEvents doc) C.$= R.renderBuilder rs

renderBytes :: ResourceUnsafeIO m => R.RenderSettings -> Document -> C.Source m ByteString
renderBytes rs doc = CL.sourceList (toEvents doc) C.$= R.renderBytes rs

renderText :: (C.ResourceThrow m, ResourceUnsafeIO m) => R.RenderSettings -> Document -> C.Source m Text
renderText rs doc = CL.sourceList (toEvents doc) C.$= R.renderText rs

fromEvents :: C.ResourceThrow m => C.Sink Event m Document
fromEvents = do
    skip EventBeginDocument
    d <- Document <$> goP <*> require goE <*> goM
    skip EventEndDocument
    y <- CL.head
    if y == Nothing
        then return d
        else lift $ C.resourceThrow $ InvalidEventStream $ "Trailing matter after epilogue: " ++ show y
  where
    skip e = do
        x <- CL.peek
        when (x == Just e) (CL.drop 1)
    many f =
        go id
      where
        go front = do
            x <- f
            case x of
                Nothing -> return $ front []
                Just y -> go (front . (:) y)
    dropReturn x = CL.drop 1 >> return x
    require f = do
        x <- f
        case x of
            Just y -> return y
            Nothing -> do
                y <- CL.head
                lift $ C.resourceThrow $ InvalidEventStream $ "Document must have a single root element, got: " ++ show y
    goP = Prologue <$> goM <*> goD <*> goM
    goM = many goM'
    goM' = do
        x <- CL.peek
        case x of
            Just (EventInstruction i) -> dropReturn $ Just $ MiscInstruction i
            Just (EventComment t) -> dropReturn $ Just $ MiscComment t
            Just (EventContent (ContentText t))
                | T.all isSpace t -> CL.drop 1 >> goM'
            _ -> return Nothing
    goD = do
        x <- CL.peek
        case x of
            Just (EventBeginDoctype name meid) -> do
                CL.drop 1
                dropTillDoctype
                return (Just $ Doctype name meid)
            _ -> return Nothing
    dropTillDoctype = do
        x <- CL.head
        case x of
            -- Leaving the following line commented so that the intention of
            -- this function stays clear. I figure in the future xml-types will
            -- be expanded again to support some form of EventDeclaration
            --
            -- Just (EventDeclaration _) -> dropTillDoctype
            Just EventEndDoctype -> return ()
            _ -> lift $ C.resourceThrow $ InvalidEventStream $ "Invalid event during doctype, got: " ++ show x
    goE = do
        x <- CL.peek
        case x of
            Just (EventBeginElement n as) -> Just <$> goE' n as
            _ -> return Nothing
    goE' n as = do
        CL.drop 1
        ns <- many goN
        y <- CL.head
        if y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else lift $ C.resourceThrow $ InvalidEventStream $ "Missing end element for " ++ show n ++ ", got: " ++ show y
    goN = do
        x <- CL.peek
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
