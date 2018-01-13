{-# LANGUAGE CPP  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | 'Enumeratee's to render XML 'Event's. Unlike libxml-enumerator and
-- expat-enumerator, this module does not provide IO and ST variants, since the
-- underlying rendering operations are pure functions.
module Text.XML.Stream.Render
    ( -- * Rendering XML files
      renderBuilder
    , renderBuilderFlush
    , renderBytes
    , renderText
    , prettify
      -- * Renderer settings
    , RenderSettings
    , def
    , rsPretty
    , rsNamespaces
    , rsAttrOrder
    , rsUseCDATA
    , rsXMLDeclaration
    , orderAttrs
      -- * Event rendering
    , tag
    , content
      -- * Attribute rendering
    , Attributes
    , attr
    , optionalAttr
    ) where

import           Blaze.ByteString.Builder
import           Control.Applicative          ((<$>))
import           Control.Monad.Trans.Resource (MonadThrow)
import           Data.ByteString              (ByteString)
import           Data.Conduit
import           Data.Conduit.Blaze           (builderToByteString)
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Default.Class           (Default (def))
import           Data.List                    (foldl')
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe, mapMaybe)
import           Data.Monoid                  (Monoid, mappend, mempty)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.XML.Types               (Content (..), Event (..),
                                               Name (..))
import           Text.XML.Stream.Token

-- | Render a stream of 'Event's into a stream of 'ByteString's. This function
-- wraps around 'renderBuilder' and 'builderToByteString', so it produces
-- optimally sized 'ByteString's with minimal buffer copying.
--
-- The output is UTF8 encoded.
--renderBytes :: Monad m => RenderSettings -> Conduit Event m ByteString
renderBytes rs = renderBuilder rs =$= builderToByteString

-- | Render a stream of 'Event's into a stream of 'Text's. This function
-- wraps around 'renderBuilder', 'builderToByteString' and 'renderBytes', so it
-- produces optimally sized 'Text's with minimal buffer copying.
{-
renderText :: (MonadThrow m)
           => RenderSettings -> Conduit Event m Text
-}
renderText rs = renderBytes rs =$= CT.decode CT.utf8

data RenderSettings = RenderSettings
    { rsPretty     :: Bool
    , rsNamespaces :: [(Text, Text)]
      -- ^ Defines some top level namespace definitions to be used, in the form
      -- of (prefix, namespace). This has absolutely no impact on the meaning
      -- of your documents, but can increase readability by moving commonly
      -- used namespace declarations to the top level.
    , rsAttrOrder  :: Name -> Map.Map Name Text -> [(Name, Text)]
      -- ^ Specify how to turn the unordered attributes used by the "Text.XML"
      -- module into an ordered list.
    , rsUseCDATA   :: Content -> Bool
      -- ^ Determines if for a given text content the renderer should use a
      -- CDATA node.
      --
      -- Default: @False@
      --
      -- @since 1.3.3
    , rsXMLDeclaration :: Bool
      -- ^ Determines whether the XML declaration will be output.
      --
      -- Default: @True@
      --
      -- @since 1.5.1
    }

instance Default RenderSettings where
    def = RenderSettings
        { rsPretty = False
        , rsNamespaces = []
        , rsAttrOrder = const Map.toList
        , rsUseCDATA = const False
        , rsXMLDeclaration = True
        }

-- | Convenience function to create an ordering function suitable for
-- use as the value of 'rsAttrOrder'. The ordering function is created
-- from an explicit ordering of the attributes, specified as a list of
-- tuples, as follows: In each tuple, the first component is the
-- 'Name' of an element, and the second component is a list of
-- attributes names. When the given element is rendered, the
-- attributes listed, when present, appear first in the given order,
-- followed by any other attributes in arbitrary order. If an element
-- does not appear, all of its attributes are rendered in arbitrary
-- order.
orderAttrs :: [(Name, [Name])] ->
              Name -> Map Name Text -> [(Name, Text)]
orderAttrs orderSpec = order
  where
    order elt attrMap =
      let initialAttrs = fromMaybe [] $ lookup elt orderSpec
          mkPair attr = (,) attr <$> Map.lookup attr attrMap
          otherAttrMap =
            Map.filterWithKey (const . not . (`elem` initialAttrs)) attrMap
      in mapMaybe mkPair initialAttrs ++ Map.toAscList otherAttrMap

-- | Render a stream of 'Event's into a stream of 'Builder's. Builders are from
-- the blaze-builder package, and allow the create of optimally sized
-- 'ByteString's with minimal buffer copying.
renderBuilder :: Monad m => RenderSettings -> Conduit Event m Builder
renderBuilder settings = CL.map Chunk =$= renderBuilder' yield' settings
  where
    yield' Flush = return ()
    yield' (Chunk bs) = yield bs

-- | Same as 'renderBuilder' but allows you to flush XML stream to ensure that all
-- events at needed point are rendered.
--
-- @since 1.3.5
renderBuilderFlush :: Monad m => RenderSettings -> Conduit (Flush Event) m (Flush Builder)
renderBuilderFlush = renderBuilder' yield

renderBuilder' :: Monad m => (Flush Builder -> Producer m o) -> RenderSettings -> Conduit (Flush Event) m o
renderBuilder' yield' settings =
    if rsPretty settings
    then prettify =$= renderEvent'
    else renderEvent'
  where
    renderEvent' = renderEvent yield' settings

renderEvent :: Monad m => (Flush Builder -> Producer m o) -> RenderSettings -> Conduit (Flush Event) m o
renderEvent yield' RenderSettings { rsPretty = isPretty, rsNamespaces = namespaces0, rsUseCDATA = useCDATA, rsXMLDeclaration = useXMLDecl } =
    loop []
  where
    loop nslevels = await >>= maybe (return ()) (go nslevels)

    go nslevels Flush = yield' Flush >> loop nslevels
    go nslevels (Chunk e) =
        case e of
            EventBeginElement n1 as -> do
                mnext <- CL.peek
                isClosed <-
                    case mnext of
                        Just (Chunk (EventEndElement n2)) | n1 == n2 -> do
                            CL.drop 1
                            return True
                        _ -> return False
                let (token, nslevels') = mkBeginToken isPretty isClosed namespaces0 nslevels n1 as
                yield' $ Chunk token
                loop nslevels'
            _ -> do
                let (token, nslevels') = eventToToken nslevels useCDATA useXMLDecl e
                yield' $ Chunk token
                loop nslevels'

eventToToken :: Stack -> (Content -> Bool) -> Bool -> Event -> (Builder, [NSLevel])
eventToToken s _ True EventBeginDocument =
    (tokenToBuilder $ TokenXMLDeclaration
            [ ("version", [ContentText "1.0"])
            , ("encoding", [ContentText "UTF-8"])
            ]
     , s)
eventToToken s _ False EventBeginDocument = (mempty, s)
eventToToken s _ _ EventEndDocument = (mempty, s)
eventToToken s _ _ (EventInstruction i) = (tokenToBuilder $ TokenInstruction i, s)
eventToToken s _ _ (EventBeginDoctype n meid) = (tokenToBuilder $ TokenDoctype n meid [], s)
eventToToken s _ _ EventEndDoctype = (mempty, s)
eventToToken s _ _ (EventCDATA t) = (tokenToBuilder $ TokenCDATA t, s)
eventToToken s _ _ (EventEndElement name) =
    (tokenToBuilder $ TokenEndElement $ nameToTName sl name, s')
  where
    (sl:s') = s
eventToToken s useCDATA _ (EventContent c)
    | useCDATA c =
        case c of
          ContentText txt -> (tokenToBuilder $ TokenCDATA txt, s)
          ContentEntity txt -> (tokenToBuilder $ TokenCDATA txt, s)
    | otherwise  = (tokenToBuilder $ TokenContent c, s)
eventToToken s _ _ (EventComment t) = (tokenToBuilder $ TokenComment t, s)
eventToToken _ _ _ EventBeginElement{} = error "eventToToken on EventBeginElement" -- mkBeginToken False s name attrs

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
prettify :: Monad m => Conduit (Flush Event) m (Flush Event)
prettify = prettify' 0

prettify' :: Monad m => Int -> Conduit (Flush Event) m (Flush Event)
prettify' level =
    await >>= maybe (return ()) goC
  where
    yield' = yield . Chunk

    goC Flush = yield Flush >> prettify' level
    goC (Chunk e) = go e

    go e@EventBeginDocument = do
        yield' e
        yield' $ EventContent $ ContentText "\n"
        prettify' level
    go e@EventBeginElement{} = do
        yield' before
        yield' e
        mnext <- CL.peek
        case mnext of
            Just (Chunk next@EventEndElement{}) -> do
                CL.drop 1
                yield' next
                yield' after
                prettify' level
            _ -> do
                yield' after
                prettify' $ level + 1
    go e@EventEndElement{} = do
        let level' = max 0 $ level - 1
        yield' $ before' level'
        yield' e
        yield' after
        prettify' level'
    go (EventContent c) = do
        cs <- takeContents (c:)
        let cs' = mapMaybe normalize cs
        case cs' of
            [] -> return ()
            _ -> do
                yield' before
                mapM_ (yield' . EventContent) cs'
                yield' after
        prettify' level
    go (EventCDATA t) = go $ EventContent $ ContentText t
    go e@EventInstruction{} = do
        yield' before
        yield' e
        yield' after
        prettify' level
    go (EventComment t) = do
        yield' before
        yield' $ EventComment $ T.concat
            [ " "
            , T.unwords $ T.words t
            , " "
            ]
        yield' after
        prettify' level

    go e@EventEndDocument = yield' e >> prettify' level
    go e@EventBeginDoctype{} = yield' e >> prettify' level
    go e@EventEndDoctype{} = yield' e >> yield' after >> prettify' level

    takeContents front = do
        me <- CL.peek
        case me of
            Just (Chunk (EventContent c)) -> do
                CL.drop 1
                takeContents $ front . (c:)
            Just (Chunk (EventCDATA t)) -> do
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


-- | Generate a complete XML 'Element'.
tag :: (Monad m) => Name -> Attributes -> Source m Event  -- ^ 'Element''s subnodes.
                                       -> Source m Event
tag name (Attributes a) content = do
  yield $ EventBeginElement name a
  content
  yield $ EventEndElement name

-- | Generate a textual 'EventContent'.
content :: (Monad m) => Text -> Source m Event
content = yield . EventContent . ContentText

-- | A list of attributes.
data Attributes = Attributes [(Name, [Content])]

instance Monoid Attributes where
  mempty = Attributes mempty
#if !MIN_VERSION_base(4,11,0)
  (Attributes a) `mappend` (Attributes b) = Attributes (a `mappend` b)
#else
instance Semigroup Attributes where
  (Attributes a) <> (Attributes b) = Attributes (a <> b)
#endif

-- | Generate a single attribute.
attr :: Name        -- ^ Attribute's name
     -> Text        -- ^ Attribute's value
     -> Attributes
attr name value = Attributes [(name, [ContentText value])]

-- | Helper function that generates a valid attribute if input isn't 'Nothing', or 'mempty' otherwise.
optionalAttr :: Name -> Maybe Text -> Attributes
optionalAttr name = maybe mempty (attr name)
