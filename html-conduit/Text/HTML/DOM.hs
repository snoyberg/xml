{-# LANGUAGE OverloadedStrings, CPP #-}
module Text.HTML.DOM
    ( eventConduit
    , sinkDoc
    , readFile
    , parseLBS
    ) where

import Prelude hiding (readFile)
import qualified Data.ByteString as S
#if MIN_VERSION_tagstream_conduit(0,5,0)
import qualified Text.HTML.TagStream.ByteString as TS
#endif
import qualified Text.HTML.TagStream as TS
import qualified Data.XML.Types as XT
import Data.Conduit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import Control.Arrow ((***), second)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
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
    TS.tokenStream =$= go []
  where
    go stack = do
        mx <- await
        case fmap (entities . fmap' (decodeUtf8With lenientDecode)) mx of
            Nothing -> closeStack stack
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

    fmap' :: (a -> b) -> TS.Token' a -> TS.Token' b
    fmap' f (TS.TagOpen x pairs b) = TS.TagOpen (f x) (map (f *** f) pairs) b
    fmap' f (TS.TagClose x) = TS.TagClose (f x)
    fmap' f (TS.Text x) = TS.Text (f x)
    fmap' f (TS.Comment x) = TS.Comment (f x)
    fmap' f (TS.Special x y) = TS.Special (f x) (f y)
    fmap' f (TS.Incomplete x) = TS.Incomplete (f x)

    entities :: TS.Token' Text -> TS.Token' Text
    entities (TS.TagOpen x pairs b) = TS.TagOpen x (map (second entities') pairs) b
    entities (TS.Text x) = TS.Text $ entities' x
    entities ts = ts

    entities' :: Text -> Text
    entities' t =
        case T.break (== '&') t of
            (_, "") -> t
            (before, t') ->
                case T.break (== ';') $ T.drop 1 t' of
                    (_, "") -> t
                    (entity, rest') ->
                        let rest = T.drop 1 rest'
                         in case decodeHtmlEntities entity of
                                XT.ContentText entity' -> T.concat [before, entity', entities' rest]
                                XT.ContentEntity _ -> T.concat [before, "&", entity, entities' rest']

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
parseLBS lbs = runIdentity $ runExceptionT_ $ CL.sourceList (L.toChunks lbs) $$ sinkDoc
