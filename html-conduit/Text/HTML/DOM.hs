{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.DOM
    ( eventConduit
    , sinkDoc
    , readFile
    , parseLBS
    , parseBSChunks
    ) where

import Control.Monad.Trans.Resource
import Prelude hiding (readFile)
import qualified Data.ByteString as S
import qualified Text.HTML.TagStream.Text as TS
import qualified Text.HTML.TagStream as TS
import qualified Data.XML.Types as XT
import Data.Conduit
import Data.Conduit.Text (decodeUtf8Lenient)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import Control.Arrow ((***), second)
import qualified Data.Set as Set
import qualified Text.XML as X
import Text.XML.Stream.Parse (decodeHtmlEntities)
import qualified Filesystem.Path.CurrentOS as F
import Data.Conduit.Binary (sourceFile)
import qualified Data.ByteString.Lazy as L
import Control.Monad.Trans.Resource (runExceptionT_)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (mapMaybe)

-- | Converts a stream of bytes to a stream of properly balanced @Event@s.
--
-- Note that there may be multiple (or not) root elements. @sinkDoc@ addresses
-- that case.
eventConduit :: Monad m => Conduit S.ByteString m XT.Event
eventConduit =
    decodeUtf8Lenient =$= TS.tokenStream =$= go []
  where
    go stack = do
        mx <- await
        case mx of
            Nothing -> closeStack stack

            -- Ignore processing instructions (or pseudo-instructions)
            Just (TS.TagOpen local _ _) | "?" `T.isPrefixOf` local -> go stack

            Just (TS.TagOpen local attrs isClosed) -> do
                let name = toName local
                    attrs' = map (toName *** return . XT.ContentText) attrs
                yield $ XT.EventBeginElement name attrs'
                if isClosed || isVoid local
                    then yield (XT.EventEndElement name) >> go stack
                    else go $ name : stack
            Just (TS.TagClose name)
                | toName name `elem` stack ->
                    let loop [] = go []
                        loop (n:ns) = do
                            yield $ XT.EventEndElement n
                            if n == toName name
                                then go ns
                                else loop ns
                     in loop stack
                | otherwise -> go stack
            Just (TS.Text t) -> do
                yield $ XT.EventContent $ XT.ContentText t
                go stack
            Just (TS.Comment t) -> do
                yield $ XT.EventComment t
                go stack
            Just TS.Special{} -> go stack
            Just TS.Incomplete{} -> go stack
    toName l = XT.Name l Nothing Nothing
    closeStack = mapM_ (yield . XT.EventEndElement)

    isVoid = flip Set.member $ Set.fromList
        [ "area"
        , "base"
        , "br"
        , "col"
        , "command"
        , "embed"
        , "hr"
        , "img"
        , "input"
        , "keygen"
        , "link"
        , "meta"
        , "param"
        , "source"
        , "track"
        , "wbr"
        ]

sinkDoc :: MonadThrow m => Sink S.ByteString m X.Document
sinkDoc =
    fmap stripDummy $ mapOutput ((,) Nothing) eventConduit =$ addDummyWrapper =$ X.fromEvents
  where
    addDummyWrapper = do
        yield (Nothing, XT.EventBeginElement "html" [])
        awaitForever yield
        yield (Nothing, XT.EventEndElement "html")

    stripDummy doc@(X.Document pro (X.Element _ _ nodes) epi) =
        case mapMaybe toElement nodes of
            [root] -> X.Document pro root epi
            _ -> doc

    toElement (X.NodeElement e) = Just e
    toElement _ = Nothing

readFile :: F.FilePath -> IO X.Document
readFile fp = runResourceT $ sourceFile (F.encodeString fp) $$ sinkDoc

parseLBS :: L.ByteString -> X.Document
parseLBS = parseBSChunks . L.toChunks

parseBSChunks :: [S.ByteString] -> X.Document
parseBSChunks bss = runIdentity $ runExceptionT_ $ CL.sourceList bss $$ sinkDoc

