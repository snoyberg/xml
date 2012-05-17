{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | 'Enumeratee's to render XML 'Event's. Unlike libxml-enumerator and
-- expat-enumerator, this module does not provide IO and ST variants, since the
-- underlying rendering operations are pure functions.
module Text.XML.Stream.Render
    ( renderBuilder
    , renderBytes
    , renderText
    , RenderSettings
    , def
    , rsPretty
    , rsNamespaces
    , prettify
    ) where

import Data.XML.Types (Event (..), Content (..), Name (..))
import Text.XML.Stream.Token
import qualified Data.Text as T
import Data.Text (Text)
import Blaze.ByteString.Builder
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Conduit as C
import Data.Conduit.Internal (sinkToPipe)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Control.Monad.Trans.Resource (MonadUnsafeIO)
import Data.Monoid (mempty)

-- | Render a stream of 'Event's into a stream of 'ByteString's. This function
-- wraps around 'renderBuilder' and 'builderToByteString', so it produces
-- optimally sized 'ByteString's with minimal buffer copying.
--
-- The output is UTF8 encoded.
renderBytes :: MonadUnsafeIO m => RenderSettings -> C.Conduit Event m ByteString
renderBytes rs = renderBuilder rs C.=$= builderToByteString

-- | Render a stream of 'Event's into a stream of 'ByteString's. This function
-- wraps around 'renderBuilder', 'builderToByteString' and 'renderBytes', so it
-- produces optimally sized 'ByteString's with minimal buffer copying.
renderText :: (C.MonadThrow m, MonadUnsafeIO m)
           => RenderSettings -> C.Conduit Event m Text
renderText rs = renderBytes rs C.=$= CT.decode CT.utf8

data RenderSettings = RenderSettings
    { rsPretty :: Bool
    , rsNamespaces :: [(Text, Text)]
      -- ^ Defines some top level namespace definitions to be used, in the form
      -- of (prefix, namespace). This has absolutely no impact on the meaning
      -- of your documents, but can increase readability by moving commonly
      -- used namespace declarations to the top level.
    }

instance Default RenderSettings where
    def = RenderSettings
        { rsPretty = False
        , rsNamespaces = []
        }

-- | Render a stream of 'Event's into a stream of 'Builder's. Builders are from
-- the blaze-builder package, and allow the create of optimally sized
-- 'ByteString's with minimal buffer copying.
renderBuilder :: Monad m => RenderSettings -> C.Conduit Event m Builder
renderBuilder RenderSettings { rsPretty = True, rsNamespaces = n } = prettify C.=$= renderBuilder' n True
renderBuilder RenderSettings { rsPretty = False, rsNamespaces = n } = renderBuilder' n False

renderBuilder' :: Monad m => [(Text, Text)] -> Bool -> C.Conduit Event m Builder
renderBuilder' namespaces0 isPretty = do
    loop []
  where
    loop nslevels = C.await >>= maybe (return ()) (go nslevels)

    go nslevels e =
        case e of
            EventBeginElement n1 as -> do
                mnext <- sinkToPipe CL.peek
                isClosed <-
                    case mnext of
                        Just (EventEndElement n2) | n1 == n2 -> do
                            sinkToPipe $ CL.drop 1
                            return True
                        _ -> return False
                let (token, nslevels') = mkBeginToken isPretty isClosed namespaces0 nslevels n1 as
                C.yield token
                loop nslevels'
            _ -> do
                let (token, nslevels') = eventToToken nslevels e
                C.yield token
                loop nslevels'

eventToToken :: Stack -> Event -> (Builder, [NSLevel])
eventToToken s EventBeginDocument =
    (tokenToBuilder $ TokenBeginDocument
            [ ("version", [ContentText "1.0"])
            , ("encoding", [ContentText "UTF-8"])
            ]
     , s)
eventToToken s EventEndDocument = (mempty, s)
eventToToken s (EventInstruction i) = (tokenToBuilder $ TokenInstruction i, s)
eventToToken s (EventBeginDoctype n meid) = (tokenToBuilder $ TokenDoctype n meid [], s)
eventToToken s EventEndDoctype = (mempty, s)
eventToToken s (EventCDATA t) = (tokenToBuilder $ TokenCDATA t, s)
eventToToken s (EventEndElement name) =
    (tokenToBuilder $ TokenEndElement $ nameToTName sl name, s')
  where
    (sl:s') = s
eventToToken s (EventContent c) = (tokenToBuilder $ TokenContent c, s)
eventToToken s (EventComment t) = (tokenToBuilder $ TokenComment t, s)
eventToToken _ EventBeginElement{} = error "eventToToken on EventBeginElement" -- mkBeginToken False s name attrs

type Stack = [NSLevel]

nameToTName :: NSLevel -> Name -> TName
nameToTName _ (Name name _ (Just pref))
    | pref == "xml" = TName (Just "xml") name
nameToTName _ (Name name Nothing _) = TName Nothing name -- invariant that this is true
nameToTName (NSLevel def' sl) (Name name (Just ns) _)
    | def' == Just ns = TName Nothing name
    | otherwise =
        case Map.lookup ns sl of
            Nothing -> error "nameToTName"
            Just pref -> TName (Just pref) name

mkBeginToken :: Bool -- ^ pretty print attributes?
             -> Bool -- ^ self closing?
             -> [(Text, Text)] -- ^ namespaces to apply to top-level
             -> Stack
             -> Name
             -> [(Name, [Content])]
             -> (Builder, Stack)
mkBeginToken isPretty isClosed namespaces0 s name attrs =
    (tokenToBuilder $ TokenBeginElement tname tattrs3 isClosed indent,
     if isClosed then s else sl3 : s)
  where
    indent = if isPretty then 2 + 4 * length s else 0
    prevsl = case s of
                [] -> NSLevel Nothing Map.empty
                sl':_ -> sl'
    (sl1, tname, tattrs1) = newElemStack prevsl name
    (sl2, tattrs2) = foldr newAttrStack (sl1, tattrs1) $ nubAttrs attrs
    (sl3, tattrs3) =
        case s of
            [] -> (sl2 { prefixes = Map.union (prefixes sl2) $ Map.fromList namespaceSL }, namespaceAttrs ++ tattrs2)
            _ -> (sl2, tattrs2)

    (namespaceSL, namespaceAttrs) = unzip $ mapMaybe unused namespaces0
    unused (k, v) =
        case lookup k' tattrs2 of
            Just{} -> Nothing
            Nothing -> Just ((v, k), (k', v'))
      where
        k' = TName (Just "xmlns") k
        v' = [ContentText v]

newElemStack :: NSLevel -> Name -> (NSLevel, TName, [TAttribute])
newElemStack nsl@(NSLevel def' _) (Name local ns _)
    | def' == ns = (nsl, TName Nothing local, [])
newElemStack (NSLevel _ nsmap) (Name local Nothing _) =
    (NSLevel Nothing nsmap, TName Nothing local, [(TName Nothing "xmlns", [])])
newElemStack (NSLevel _ nsmap) (Name local (Just ns) Nothing) =
    (NSLevel (Just ns) nsmap, TName Nothing local, [(TName Nothing "xmlns", [ContentText ns])])
newElemStack (NSLevel def' nsmap) (Name local (Just ns) (Just pref)) =
    case Map.lookup ns nsmap of
        Just pref'
            | pref == pref' ->
                ( NSLevel def' nsmap
                , TName (Just pref) local
                , []
                )
        _ -> ( NSLevel def' nsmap'
             , TName (Just pref) local
             , [(TName (Just "xmlns") pref, [ContentText ns])]
             )
  where
    nsmap' = Map.insert ns pref nsmap

newAttrStack :: (Name, [Content]) -> (NSLevel, [TAttribute]) -> (NSLevel, [TAttribute])
newAttrStack (name, value) (NSLevel def' nsmap, attrs) =
    (NSLevel def' nsmap', addNS $ (tname, value) : attrs)
  where
    (nsmap', tname, addNS) =
        case name of
            Name local Nothing _ -> (nsmap, TName Nothing local, id)
            Name local (Just ns) mpref ->
                let ppref = fromMaybe "ns" mpref
                    (pref, addNS') = getPrefix ppref nsmap ns
                 in (Map.insert ns pref nsmap, TName (Just pref) local, addNS')

getPrefix :: Text -> Map Text Text -> Text -> (Text, [TAttribute] -> [TAttribute])
getPrefix _ _ "http://www.w3.org/XML/1998/namespace" = ("xml", id)
getPrefix ppref nsmap ns =
    case Map.lookup ns nsmap of
        Just pref -> (pref, id)
        Nothing ->
            let pref = findUnused ppref $ Map.elems nsmap
             in (pref, (:) (TName (Just "xmlns") pref, [ContentText ns]))
  where
    findUnused x xs
        | x `elem` xs = findUnused (x `T.snoc` '_') xs
        | otherwise = x

-- | Convert a stream of 'Event's into a prettified one, adding extra
-- whitespace. Note that this can change the meaning of your XML.
prettify :: Monad m => C.Conduit Event m Event
prettify = prettify' 0

prettify' :: Monad m => Int -> C.Conduit Event m Event
prettify' level = do
    me <- C.await
    case me of
        Nothing -> return ()
        Just e -> go e
  where
    go e@EventBeginDocument = do
        C.yield e
        C.yield $ EventContent $ ContentText "\n"
        prettify' level
    go e@EventBeginElement{} = do
        C.yield before
        C.yield e
        mnext <- sinkToPipe CL.peek
        case mnext of
            Just next@EventEndElement{} -> do
                sinkToPipe $ CL.drop 1
                C.yield next
                C.yield after
                prettify' level
            _ -> do
                C.yield after
                prettify' $ level + 1
    go e@EventEndElement{} = do
        let level' = max 0 $ level - 1
        C.yield $ before' level'
        C.yield e
        C.yield after
        prettify' level'
    go (EventContent c) = do
        cs <- sinkToPipe $ takeContents (c:)
        let cs' = mapMaybe normalize cs
        case cs' of
            [] -> return ()
            _ -> do
                C.yield before
                mapM_ (C.yield . EventContent) cs'
                C.yield after
        prettify' level
    go (EventCDATA t) = go $ EventContent $ ContentText t
    go e@EventInstruction{} = do
        C.yield before
        C.yield e
        C.yield after
        prettify' level
    go (EventComment t) = do
        C.yield before
        C.yield $ EventComment $ T.concat
            [ " "
            , T.unwords $ T.words t
            , " "
            ]
        C.yield after
        prettify' level

    go e@EventEndDocument = C.yield e >> prettify' level
    go e@EventBeginDoctype{} = C.yield e >> prettify' level
    go e@EventEndDoctype{} = C.yield e >> C.yield after >> prettify' level

    takeContents front = do
        me <- CL.peek
        case me of
            Just (EventContent c) -> do
                CL.drop 1
                takeContents $ front . (c:)
            Just (EventCDATA t) -> do
                CL.drop 1
                takeContents $ front . (ContentText t:)
            _ -> return $ front []

    normalize (ContentText t)
        | T.null t' = Nothing
        | otherwise = Just $ ContentText t'
      where
        t' = T.unwords $ T.words t
    normalize c = Just c

    before = EventContent $ ContentText $ T.replicate level "    "
    before' l = EventContent $ ContentText $ T.replicate l "    "
    after = EventContent $ ContentText "\n"

nubAttrs :: [(Name, v)] -> [(Name, v)]
nubAttrs orig =
    front []
  where
    (front, _) = foldl' go (id, Set.empty) orig
    go (dlist, used) (k, v)
        | k `Set.member` used = (dlist, used)
        | otherwise = (dlist . ((k, v):), Set.insert k used)
