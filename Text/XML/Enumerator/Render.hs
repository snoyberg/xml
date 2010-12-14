{-# LANGUAGE OverloadedStrings #-}
-- | 'Enumeratee's to render XML 'Event's. Unlike libxml-enumerator and
-- expat-enumerator, this module does not provide IO and ST variants, since the
-- underlying rendering operations are pure functions.
module Text.XML.Enumerator.Render
    ( renderBuilder
    , renderBytes
    ) where

import Data.XML.Types ( Event (..), Content (..), Name (..), Attribute (..)
                      , Doctype (..))
import Text.XML.Enumerator.Token
import qualified Data.Enumerator as E
import Data.Enumerator ((>>==), ($$))
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO)

-- | Render a stream of 'Event's into a stream of 'ByteString's. This function
-- wraps around 'renderBuilder' and 'builderToByteString', so it produces
-- optimally sized 'ByteString's with minimal buffer copying.
renderBytes :: MonadIO m => E.Enumeratee Event ByteString m b
renderBytes s = E.joinI $ renderBuilder $$ builderToByteString s

-- | Render a stream of 'Event's into a stream of 'Builder's. Builders are from
-- the blaze-builder package, and allow the create of optimally sized
-- 'ByteString's with minimal buffer copying.
renderBuilder :: Monad m => E.Enumeratee Event Builder m b
renderBuilder =
    loop []
  where
    loop stack = E.checkDone $ step stack
    step stack k = do
        x <- E.head
        case x of
            Nothing -> E.yield (E.Continue k) E.EOF
            Just (EventBeginElement name as) -> do
                x' <- E.peek
                if x' == Just (EventEndElement name)
                    then do
                        E.drop 1
                        go $ mkBeginToken True stack name as
                    else go $ mkBeginToken False stack name as
            Just e -> go $ eventToToken stack e
      where
        go (ts, stack') = k (E.Chunks $ map tokenToBuilder $ ts []) >>== loop stack'

eventToToken :: Stack -> Event -> ([Token] -> [Token], [NSLevel])
eventToToken s EventBeginDocument =
    ((:) (TokenBeginDocument
            [ ("version", [ContentText "1.0"])
            , ("encoding", [ContentText "UTF-8"])
            ])
     , s)
eventToToken s EventEndDocument = (id, s)
eventToToken s (EventInstruction i) = ((:) (TokenInstruction i), s)
eventToToken s (EventDoctype (Doctype n meid _)) =
    ((:) (TokenDoctype n meid), s)
eventToToken s (EventBeginElement name attrs) = mkBeginToken False s name attrs
eventToToken s (EventEndElement name) =
    ((:) (TokenEndElement $ nameToTName sl name), s')
  where
    (sl:s') = s
eventToToken s (EventContent c) = ((:) (TokenContent c), s)
eventToToken s (EventComment t) = ((:) (TokenComment t), s)

type Stack = [NSLevel]

nameToTName :: NSLevel -> Name -> TName
nameToTName _ (Name name _ (Just pref))
    | pref == "xml" = TName (Just "xml") name
nameToTName _ (Name name Nothing _) = TName Nothing name -- invariant that this is true
nameToTName (NSLevel def sl) (Name name (Just ns) _)
    | def == Just ns = TName Nothing name
    | otherwise =
        case Map.lookup ns sl of
            Nothing -> error "nameToTName"
            Just pref -> TName (Just pref) name

mkBeginToken :: Bool -> Stack -> Name -> [Attribute]
             -> ([Token] -> [Token], Stack)
mkBeginToken isClosed s name attrs =
    ((:) (TokenBeginElement tname tattrs2 isClosed),
     if isClosed then s else sl2 : s)
  where
    prevsl = case s of
                [] -> NSLevel Nothing Map.empty
                sl':_ -> sl'
    (sl1, tname, tattrs1) = newElemStack prevsl name
    (sl2, tattrs2) = foldr newAttrStack (sl1, tattrs1) attrs

newElemStack :: NSLevel -> Name -> (NSLevel, TName, [TAttribute])
newElemStack nsl@(NSLevel def _) (Name local ns _)
    | def == ns = (nsl, TName Nothing local, [])
newElemStack (NSLevel _ nsmap) (Name local Nothing _) =
    (NSLevel Nothing nsmap, TName Nothing local, [(TName Nothing "xmlns", [])])
newElemStack (NSLevel _ nsmap) (Name local (Just ns) Nothing) =
    (NSLevel (Just ns) nsmap, TName Nothing local, [(TName Nothing "xmlns", [ContentText ns])])
newElemStack (NSLevel def nsmap) (Name local (Just ns) (Just pref)) =
    (NSLevel def nsmap', TName (Just pref) local, [(TName (Just "xmlns") pref, [ContentText ns])])
  where
    nsmap' = Map.insert ns pref nsmap

newAttrStack :: Attribute -> (NSLevel, [TAttribute]) -> (NSLevel, [TAttribute])
newAttrStack (Attribute name value) (NSLevel def nsmap, attrs) =
    (NSLevel def nsmap', addNS $ (tname, value) : attrs)
  where
    (nsmap', tname, addNS) =
        case name of
            Name local Nothing _ -> (nsmap, TName Nothing local, id)
            Name local (Just ns) mpref ->
                let ppref = fromMaybe "ns" mpref
                    (pref, addNS') = getPrefix ppref nsmap ns
                 in (Map.insert ns pref nsmap, TName (Just pref) local, addNS')

getPrefix :: Text -> Map Text Text -> Text -> (Text, [TAttribute] -> [TAttribute])
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
