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
import Data.Char (isSpace)
import Data.Default (Default (def))
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Conduit as C
import Data.Conduit.Internal (sinkToPipe)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Control.Exception (assert)
import Control.Monad.Trans.Resource (MonadUnsafeIO)

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
    }

instance Default RenderSettings where
    def = RenderSettings
        { rsPretty = False
        }

-- | Render a stream of 'Event's into a stream of 'Builder's. Builders are from
-- the blaze-builder package, and allow the create of optimally sized
-- 'ByteString's with minimal buffer copying.
renderBuilder :: Monad m => RenderSettings -> C.Conduit Event m Builder
renderBuilder RenderSettings { rsPretty = True } = prettify C.=$= renderBuilder' True
renderBuilder RenderSettings { rsPretty = False } = renderBuilder' False

renderBuilder' :: Monad m => Bool -> C.Conduit Event m Builder
renderBuilder' isPretty = C.conduitState
    (id, [])
    push
    close
  where
    go' front = map tokenToBuilder $ front []
    go stack _ [] front = (stack, id, go' front)
    -- we want to wait and see if the next event is the matching end
    go stack False [e@EventBeginElement{}] front =
        (stack, (e:), go' front)
    go stack atEnd
        ( EventBeginElement n1 as
        : EventEndElement n2
        : rest
        ) front | n1 == n2 =
            let (token, stack') = mkBeginToken isPretty True stack n1 as
             in go stack' atEnd rest (front . token)
    go stack atEnd (EventBeginElement name as:rest) front =
        let (token, stack') = mkBeginToken isPretty False stack name as
         in go stack' atEnd rest (front . token)
    go stack atEnd (e:rest) front =
        let (token, stack') = eventToToken stack e
         in go stack' atEnd rest (front . token)

    push (front, stack) es =
        return $ C.StateProducing (leftover, stack') ts
      where
        (stack', leftover, ts) = go stack False (front [es]) id

    close (front, stack) =
        return ts
      where
        (_, _leftover, ts) = go stack True (front []) id

eventToToken :: Stack -> Event -> ([Token] -> [Token], [NSLevel])
eventToToken s EventBeginDocument =
    ((:) (TokenBeginDocument
            [ ("version", [ContentText "1.0"])
            , ("encoding", [ContentText "UTF-8"])
            ])
     , s)
eventToToken s EventEndDocument = (id, s)
eventToToken s (EventInstruction i) = ((:) (TokenInstruction i), s)
eventToToken s (EventBeginDoctype n meid) = ((:) (TokenDoctype n meid []), s)
eventToToken s EventEndDoctype = (id, s)
eventToToken s (EventCDATA t) = ((:) (TokenCDATA t), s)
eventToToken s (EventEndElement name) =
    ((:) (TokenEndElement $ nameToTName sl name), s')
  where
    (sl:s') = s
eventToToken s (EventContent c) = ((:) (TokenContent c), s)
eventToToken s (EventComment t) = ((:) (TokenComment t), s)
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
             -> Bool -> Stack -> Name -> [(Name, [Content])]
             -> ([Token] -> [Token], Stack)
mkBeginToken isPretty isClosed s name attrs =
    ((:) (TokenBeginElement tname tattrs2 isClosed indent),
     if isClosed then s else sl2 : s)
  where
    indent = if isPretty then 2 + 4 * length s else 0
    prevsl = case s of
                [] -> NSLevel Nothing Map.empty
                sl':_ -> sl'
    (sl1, tname, tattrs1) = newElemStack prevsl name
    (sl2, tattrs2) = foldr newAttrStack (sl1, tattrs1) $ nubAttrs attrs

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

    go e = C.yield e >> prettify' level

    takeContents front = do
        me <- CL.peek
        case me of
            Just (EventContent c) -> do
                CL.drop 1
                takeContents $ front . (c:)
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
    {-
    (id, (level0, names0))
    push
    close
  where
    push (front, a) b = do
        let (a', es) = go False a (front [b]) id
        return $ C.StateProducing a' es
    close (front, a) = do
        let ((front', _), es) = go True a (front []) id
        assert (null $ front' [])
            $ return es

    go _ state [] front = ((id, state), front [])
    go atEnd state@(level, _) es@(EventContent t:xs) front =
        case takeContents (t:) xs of
            Nothing
                | not atEnd -> (((es++), state), front [])
                | otherwise -> assert False $ error "Text.XML.Stream.Render.prettify'"
            Just (ts, xs') ->
                let ts' = map EventContent $ cleanWhite ts
                    ts'' = if null ts' then [] else before level : ts' ++ [after]
                 in go atEnd state xs' (front . (ts'' ++))
    go atEnd (level, names) (x:xs) front = do
        go atEnd (level', names') xs' (front . chunks)
      where
        (chunks, level', names', xs') =
            case (x, xs) of
                (EventBeginElement name attrs, EventEndElement _:rest) ->
                    (\a -> before level : EventBeginElement name attrs : EventEndElement name : after : a, level, names, rest)
                (EventBeginElement name attrs, _) ->
                    (\a -> before level : EventBeginElement name attrs : after : a, level + 1, name : names, xs)
                (EventEndElement _, _) ->
                    let newLevel = level - 1
                        n:ns = names
                     in (\a -> before newLevel : EventEndElement n : after : a, newLevel, ns, xs)
                (EventBeginDocument, _) -> ((EventBeginDocument:), level, names, xs)
                (EventEndDocument, _) -> (\a -> EventEndDocument : a, level, names, xs)
                (EventComment t, _) -> (\a -> before level : EventComment (T.map normalSpace t) : after : a, level, names, xs)
                (e, _) -> (\a -> before level : e : after : a, level, names, xs)

    -}

takeContents :: ([Content] -> [Content]) -> [Event] -> Maybe ([Content], [Event])
takeContents _ [] = Nothing
takeContents front (EventContent t:es) = takeContents (front . (t:)) es
takeContents front es = Just (front [], es)

normalSpace :: Char -> Char
normalSpace c
    | isSpace c = ' '
    | otherwise = c

cleanWhite :: [Content] -> [Content]
cleanWhite x =
    go True [] $ go True [] x
  where
    go _ end (ContentEntity e:rest) = go False (ContentEntity e : end) rest
    go isFront end (ContentText t:rest) =
        if T.null t'
            then go isFront end rest
            else go False (ContentText t' : end) rest
      where
        t' = (if isFront then T.dropWhile isSpace else id) $ T.map normalSpace t
    go _ end [] = end

nubAttrs :: [(Name, v)] -> [(Name, v)]
nubAttrs orig =
    front []
  where
    (front, _) = foldl' go (id, Set.empty) orig
    go (dlist, used) (k, v)
        | k `Set.member` used = (dlist, used)
        | otherwise = (dlist . ((k, v):), Set.insert k used)
