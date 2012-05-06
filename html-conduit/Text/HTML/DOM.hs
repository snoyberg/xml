{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.DOM
    ( eventConduit
    , sinkDoc
    , readFile
    , parseLBS
    ) where

import Prelude hiding (readFile)
import qualified Data.ByteString as S
import qualified Text.HTML.TagStream as TS
import qualified Data.XML.Types as XT
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Arrow ((***))
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Set as Set
import qualified Text.XML as X
import qualified Filesystem.Path.CurrentOS as F
import Data.Conduit.Filesystem (sourceFile)
import qualified Data.ByteString.Lazy as L
import Control.Monad.Trans.Resource (runExceptionT_)
import Data.Functor.Identity (runIdentity)

-- | Converts a stream of bytes to a stream of properly balanced @Event@s.
eventConduit :: Monad m => Conduit S.ByteString m XT.Event
eventConduit =
    TS.tokenStream =$= go []
  where
    go stack = do
        mx <- await
        case fmap (fmap' $ decodeUtf8With lenientDecode) mx of
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
sinkDoc = eventConduit =$ X.fromEvents

readFile :: F.FilePath -> IO X.Document
readFile fp = runResourceT $ sourceFile fp $$ sinkDoc

parseLBS :: L.ByteString -> X.Document
parseLBS lbs = runIdentity $ runExceptionT_ $ CL.sourceList (L.toChunks lbs) $$ sinkDoc
