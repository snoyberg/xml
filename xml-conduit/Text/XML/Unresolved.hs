{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
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
      -- * Text
    , parseText
    , parseText_
    , sinkTextDoc
      -- * Byte streams
    , sinkDoc
      -- * Streaming functions
    , toEvents
    , elementToEvents
    , fromEvents
    , elementFromEvents
    , renderBuilder
    , renderBytes
    , renderText
      -- * Exceptions
    , InvalidEventStream (..)
      -- * Settings
    , P.def
      -- ** Parse
    , P.ParseSettings
    , P.psDecodeEntities
    , P.psRetainNamespaces
      -- ** Render
    , R.RenderSettings
    , R.rsPretty
    , R.rsNamespaces
    ) where

import           Conduit
import           Control.Applicative          ((<$>), (<*>))
import           Control.Exception            (Exception, SomeException, throw)
import           Control.Monad                (when)
import           Control.Monad.Trans.Class    (lift)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Lazy         as L
import           Data.Char                    (isSpace)
import qualified Data.Conduit.Binary          as CB
import           Data.Conduit.Lazy            (lazyConsume)
import qualified Data.Conduit.List            as CL
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Data.Typeable                (Typeable)
import           Data.XML.Types
import           Prelude                      hiding (readFile, writeFile)
import           System.IO.Unsafe             (unsafePerformIO)
import           Text.XML.Stream.Parse        (ParseSettings)
import qualified Text.XML.Stream.Parse        as P
import qualified Text.XML.Stream.Render       as R

readFile :: P.ParseSettings -> FilePath -> IO Document
readFile ps fp = runConduitRes $ CB.sourceFile fp .| sinkDoc ps

sinkDoc :: MonadThrow m
        => P.ParseSettings
        -> ConduitT ByteString o m Document
sinkDoc ps = P.parseBytesPos ps .| fromEvents

writeFile :: R.RenderSettings -> FilePath -> Document -> IO ()
writeFile rs fp doc =
    runConduitRes $ renderBytes rs doc .| CB.sinkFile fp

renderLBS :: R.RenderSettings -> Document -> L.ByteString
renderLBS rs doc =
    L.fromChunks $ unsafePerformIO
                 -- not generally safe, but we know that runResourceT
                 -- will not deallocate any of the resources being used
                 -- by the process
                 $ lazyConsume
                 $ renderBytes rs doc

parseLBS :: P.ParseSettings -> L.ByteString -> Either SomeException Document
parseLBS ps lbs = runConduit $ CL.sourceList (L.toChunks lbs) .| sinkDoc ps

parseLBS_ :: P.ParseSettings -> L.ByteString -> Document
parseLBS_ ps lbs = either throw id $ parseLBS ps lbs

data InvalidEventStream = ContentAfterRoot P.EventPos
                        | MissingRootElement
                        | InvalidInlineDoctype P.EventPos
                        | MissingEndElement Name (Maybe P.EventPos)
                        | UnterminatedInlineDoctype
    deriving Typeable
instance Exception InvalidEventStream
instance Show InvalidEventStream where
    show (ContentAfterRoot (pos, e)) = mShowPos pos ++ "Found content after root element: " ++ prettyShowE e
    show MissingRootElement = "Missing root element"
    show (InvalidInlineDoctype (pos, e)) = mShowPos pos ++ "Invalid content inside doctype: " ++ prettyShowE e
    show (MissingEndElement name Nothing) = "Documented ended while expected end element for: " ++ prettyShowName name
    show (MissingEndElement name (Just (pos, e))) = mShowPos pos ++ "Expected end element for: " ++ prettyShowName name ++ ", but received: " ++ prettyShowE e
    show UnterminatedInlineDoctype = "Unterminated doctype declaration"

mShowPos :: Maybe P.PositionRange -> String
mShowPos Nothing    = ""
mShowPos (Just pos) = show pos ++ ": "

prettyShowE :: Event -> String
prettyShowE = show -- FIXME

prettyShowName :: Name -> String
prettyShowName = show -- FIXME

renderBuilder :: Monad m => R.RenderSettings -> Document -> ConduitT i Builder m ()
renderBuilder rs doc = CL.sourceList (toEvents doc) .| R.renderBuilder rs

renderBytes :: PrimMonad m => R.RenderSettings -> Document -> ConduitT i ByteString m ()
renderBytes rs doc = CL.sourceList (toEvents doc) .| R.renderBytes rs

renderText :: (MonadThrow m, PrimMonad m) => R.RenderSettings -> Document -> ConduitT i Text m ()
renderText rs doc = CL.sourceList (toEvents doc) .| R.renderText rs

manyTries :: Monad m => m (Maybe a) -> m [a]
manyTries f =
    go id
  where
    go front = do
        x <- f
        case x of
            Nothing -> return $ front []
            Just y  -> go (front . (:) y)

dropReturn :: Monad m => a -> ConduitM i o m a
dropReturn x = CL.drop 1 >> return x

-- | Parse a document from a stream of events.
fromEvents :: MonadThrow m => ConduitT P.EventPos o m Document
fromEvents = do
    skip EventBeginDocument
    d <- Document <$> goP <*> require elementFromEvents <*> goM
    skip EventEndDocument
    y <- CL.head
    case y of
        Nothing -> return d
        Just (_, EventEndDocument) -> lift $ throwM MissingRootElement
        Just z ->
            lift $ throwM $ ContentAfterRoot z
  where
    skip e = do
        x <- CL.peek
        when (fmap snd x == Just e) (CL.drop 1)
    require f = do
        x <- f
        case x of
            Just y -> return y
            Nothing -> do
                my <- CL.head
                case my of
                    Nothing -> error "Text.XML.Unresolved:impossible"
                    Just (_, EventEndDocument) -> lift $ throwM MissingRootElement
                    Just y -> lift $ throwM $ ContentAfterRoot y
    goP = Prologue <$> goM <*> goD <*> goM
    goM = manyTries goM'
    goM' = do
        x <- CL.peek
        case x of
            Just (_, EventInstruction i) -> dropReturn $ Just $ MiscInstruction i
            Just (_, EventComment t) -> dropReturn $ Just $ MiscComment t
            Just (_, EventContent (ContentText t))
                | T.all isSpace t -> CL.drop 1 >> goM'
            _ -> return Nothing
    goD = do
        x <- CL.peek
        case x of
            Just (_, EventBeginDoctype name meid) -> do
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
            Just (_, EventEndDoctype) -> return ()
            Just epos -> lift $ throwM $ InvalidInlineDoctype epos
            Nothing -> lift $ throwM UnterminatedInlineDoctype

-- | Try to parse a document element (as defined in XML) from a stream of events.
--
-- @since 1.3.5
elementFromEvents :: MonadThrow m => ConduitT P.EventPos o m (Maybe Element)
elementFromEvents = goE
  where
    goE = do
        x <- CL.peek
        case x of
            Just (_, EventBeginElement n as) -> Just <$> goE' n as
            _                                -> return Nothing
    goE' n as = do
        CL.drop 1
        ns <- manyTries goN
        y <- CL.head
        if fmap snd y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else lift $ throwM $ MissingEndElement n y
    goN = do
        x <- CL.peek
        case x of
            Just (_, EventBeginElement n as) -> (Just . NodeElement) <$> goE' n as
            Just (_, EventInstruction i) -> dropReturn $ Just $ NodeInstruction i
            Just (_, EventContent c) -> dropReturn $ Just $ NodeContent c
            Just (_, EventComment t) -> dropReturn $ Just $ NodeComment t
            Just (_, EventCDATA t) -> dropReturn $ Just $ NodeContent $ ContentText t
            _ -> return Nothing

-- | Render a document into events.
toEvents :: Document -> [Event]
toEvents (Document prol root epi) =
      (EventBeginDocument :)
    . goP prol . elementToEvents' root . goM epi $ [EventEndDocument]
  where
    goP (Prologue before doctype after) =
        goM before . maybe id goD doctype . goM after
    goM []     = id
    goM [x]    = (goM' x :)
    goM (x:xs) = (goM' x :) . goM xs
    goM' (MiscInstruction i) = EventInstruction i
    goM' (MiscComment t)     = EventComment t
    goD (Doctype name meid) =
        (:) (EventBeginDoctype name meid)
      . (:) EventEndDoctype

-- | Render a document element into events.
--
-- @since 1.3.5
elementToEvents :: Element -> [Event]
elementToEvents e = elementToEvents' e []

elementToEvents' :: Element -> [Event] -> [Event]
elementToEvents' = goE
  where
    goE (Element name as ns) =
          (EventBeginElement name as :)
        . goN ns
        . (EventEndElement name :)
    goN []     = id
    goN [x]    = goN' x
    goN (x:xs) = goN' x . goN xs
    goN' (NodeElement e)     = goE e
    goN' (NodeInstruction i) = (EventInstruction i :)
    goN' (NodeContent c)     = (EventContent c :)
    goN' (NodeComment t)     = (EventComment t :)

compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
    compressNodes $ NodeContent (ContentText $ x `T.append` y) : z
compressNodes (x:xs) = x : compressNodes xs

parseText :: ParseSettings -> TL.Text -> Either SomeException Document
parseText ps tl =
    runConduit
  $ CL.sourceList (TL.toChunks tl)
 .| sinkTextDoc ps

parseText_ :: ParseSettings -> TL.Text -> Document
parseText_ ps = either throw id . parseText ps

sinkTextDoc :: MonadThrow m
            => ParseSettings
            -> ConduitT Text o m Document
sinkTextDoc ps = P.parseTextPos ps .| fromEvents
