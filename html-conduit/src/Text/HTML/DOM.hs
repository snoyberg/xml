{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.DOM
    ( eventConduit
    , sinkDoc
    , readFile
    , parseLBS
    , parseBSChunks
    , eventConduitText
    , sinkDocText
    , parseLT
    , parseSTChunks
    ) where

import Control.Monad.Trans.Resource
import Prelude hiding (readFile)
import qualified Data.ByteString as S
import qualified Text.HTML.TagStream.Text as TS
import qualified Data.XML.Types as XT
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Conduit.List as CL
import Control.Arrow ((***))
import qualified Data.Set as Set
import qualified Text.XML as X
import Conduit
import qualified Data.ByteString.Lazy as L
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

-- | Converts a stream of bytes to a stream of properly balanced @Event@s.
--
-- Note that there may be multiple (or not) root elements. @sinkDoc@ addresses
-- that case.
eventConduit :: Monad m => ConduitT S.ByteString XT.Event m ()
eventConduit = decodeUtf8LenientC .| eventConduit'

eventConduitText :: Monad m => ConduitT T.Text XT.Event m ()
eventConduitText = eventConduit'

eventConduit' :: Monad m => ConduitT T.Text XT.Event m ()
eventConduit' =
    TS.tokenStream .| go []
  where
    go stack = do
        mx <- await
        case mx of
            Nothing -> closeStack stack

            -- Ignore processing instructions (or pseudo-instructions)
            Just (TS.TagOpen local _ _) | "?" `T.isPrefixOf` local -> go stack

            Just (TS.TagOpen local attrs isClosed) -> do
                let name = toName local
                    attrs' = map (toName *** return . XT.ContentText) $ Map.toList attrs
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

sinkDoc :: MonadThrow m => ConduitT S.ByteString o m X.Document
sinkDoc = sinkDoc' eventConduit

sinkDocText :: MonadThrow m => ConduitT T.Text o m X.Document
sinkDocText = sinkDoc' eventConduitText

sinkDoc'
  :: MonadThrow m
  => ConduitT a XT.Event m ()
  -> ConduitT a o m X.Document
sinkDoc' f =
    fmap stripDummy $ mapOutput ((,) Nothing) f .| addDummyWrapper .| X.fromEvents
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

readFile :: FilePath -> IO X.Document
readFile fp = withSourceFile fp $ \src -> runConduit $ src .| sinkDoc

parseLBS :: L.ByteString -> X.Document
parseLBS = parseBSChunks . L.toChunks

parseBSChunks :: [S.ByteString] -> X.Document
parseBSChunks tss =
  case runConduit $ CL.sourceList tss .| sinkDoc of
    Left e -> error $ "Unexpected exception in parseBSChunks: " ++ show e
    Right x -> x

parseLT :: TL.Text -> X.Document
parseLT = parseSTChunks . TL.toChunks

parseSTChunks :: [T.Text] -> X.Document
parseSTChunks tss =
  case runConduit $ CL.sourceList tss .| sinkDocText of
    Left e -> error $ "Unexpected exception in parseSTChunks: " ++ show e
    Right x -> x

