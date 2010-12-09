{-# LANGUAGE OverloadedStrings #-}
-- | 'Enumeratee's to render XML 'Event's. Unlike libxml-enumerator and
-- expat-enumerator, this module does not provide IO and ST variants, since the
-- underlying rendering operations are pure functions.
module Text.XML.Enumerator.Render
    ( renderBuilder
    ) where

import Data.XML.Types ( Event (..), Content (..), Name (..), Attribute (..)
                      , Doctype (..))
import Text.XML.Enumerator.Token
import qualified Data.Enumerator as E
import Data.Enumerator ((>>==))
import qualified Data.Text.Lazy as T
import Blaze.ByteString.Builder
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

renderBuilder :: Monad m => E.Enumeratee Event Builder m b
renderBuilder =
    loop []
  where
    loop stack = E.checkDone $ step stack
    step stack k = do
        x <- E.head
        case x of
            Nothing -> E.yield (E.Continue k) E.EOF
            Just e@(EventBeginElement name as) -> do
                x' <- E.peek
                if x' == Just (EventEndElement name)
                    then do
                        E.drop 1
                        go $ mkBeginToken True stack name as
                    else go $ mkBeginToken False stack name as
            Just e -> go $ eventToToken stack e
      where
        go (ts, stack') = k (E.Chunks $ map tokenToBuilder $ ts []) >>== loop stack'

eventToToken :: Stack -> Event -> ([Token] -> [Token], [StackLevel])
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

type Stack = [StackLevel]
type StackLevel = Map T.Text T.Text

newStack :: StackLevel -> [Name] -> (StackLevel, [TAttribute])
newStack sl =
    foldr go (sl, [])
  where
    go (Name _ Nothing _) (m, tattrs) = (m, tattrs)
    go (Name _ (Just ns) mpref) (m, tattrs) =
        case Map.lookup ns m of
            Just _ -> (m, tattrs)
            Nothing -> go' $ fromMaybe "x" mpref
      where
        go' pref =
            if pref `elem` Map.elems m
                then go' $ T.append pref "x"
                else (Map.insert ns pref m, newAttr ns pref : tattrs)
        newAttr ns' pref = (TName (Just "xmlns") pref, [ContentText ns'])

nameToTName :: StackLevel -> Name -> TName
nameToTName _ (Name name _ (Just pref))
    | pref == "xml" = TName (Just "xml") name
nameToTName _ (Name name Nothing _) = TName Nothing name
nameToTName sl (Name name (Just ns) _) =
    case Map.lookup ns sl of
        Nothing -> error "nameToTName"
        Just pref -> TName (Just pref) name

attrToTAttr :: StackLevel -> Attribute -> TAttribute
attrToTAttr sl (Attribute key val) = (nameToTName sl key, val)

mkBeginToken isClosed s name attrs =
    ((:) (TokenBeginElement (nameToTName sl name) (map (attrToTAttr sl) attrs ++ tattrs) isClosed),
     if isClosed then s else sl : s)
  where
    names = name : map (\(Attribute n _) -> n) attrs
    prevsl = case s of
                [] -> Map.empty
                sl':_ -> sl'
    (sl, tattrs) = newStack prevsl names
