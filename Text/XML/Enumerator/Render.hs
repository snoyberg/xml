{-# LANGUAGE OverloadedStrings #-}
-- | 'Enumeratee's to render XML 'Event's. Unlike libxml-enumerator and
-- expat-enumerator, this module does not provide IO and ST variants, since the
-- underlying rendering operations are pure functions.
module Text.XML.Render.Enumerator
    ( {-renderText
    , renderTextLazy
    , renderBytes
    , renderBytesLazy
    -}
      renderBuilder
    ) where

import Data.XML.Types ( Event (..), Content (..), Name (..), Attribute (..)
                      , Doctype (..))
import Text.XML.Enumerator.Token
import qualified Data.Enumerator as E
import Data.Enumerator ((>>==))
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text as TS
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

renderBuilder :: Monad m => E.Enumeratee Event Builder m b
renderBuilder =
    loop []
  where
    loop stack = E.checkDone $ E.continue . step stack
    step _ k E.EOF = E.yield (E.Continue k) E.EOF
    step stack k (E.Chunks []) = E.continue $ step stack k
    step stackInit k (E.Chunks events) =
        k (E.Chunks $ map tokenToBuilder tokens) >>== loop stack'
      where
        (tokens, stack') = eventsToTokens stackInit events
        eventsToTokens stack [] = ([], stack)
        eventsToTokens stack (e:es) =
            let (t1, stack'') = eventToToken stack e
                (t2, stack''') = eventsToTokens stack'' es
             in (t1 t2, stack''')

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
eventToToken s (EventBeginElement name attrs) =
    ((:) (TokenBeginElement (nameToTName sl name) (map (attrToTAttr sl) attrs ++ tattrs) False), sl : s)
  where
    names = name : map (\(Attribute n _) -> n) attrs
    prevsl = case s of
                [] -> Map.empty
                sl:_ -> sl
    (sl, tattrs) = newStack prevsl names
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
nameToTName _ (Name name Nothing _) = TName Nothing name
nameToTName sl (Name name (Just ns) _) =
    case Map.lookup ns sl of
        Nothing -> error "nameToTName"
        Just pref -> TName (Just pref) name

attrToTAttr :: StackLevel -> Attribute -> TAttribute
attrToTAttr sl (Attribute key val) = (nameToTName sl key, val)

{-
renderText :: Monad m => E.Enumeratee Event TS.Text m b
renderText = concatMap' $ T.toChunks . eventToText

-- | Performs UTF-8 encoding.
renderBytesLazy :: Monad m => E.Enumeratee Event L.ByteString m b
renderBytesLazy = E.map $ T.encodeUtf8 . eventToText

-- | Performs UTF-8 encoding.
renderBytes :: Monad m => E.Enumeratee Event S.ByteString m b
renderBytes = concatMap' $ L.toChunks . T.encodeUtf8 . eventToText
-}

concatMap' :: Monad m => (ao -> [ai]) -> E.Enumeratee ao ai m b
concatMap' f =
    loop
  where
    loop = E.checkDone $ E.continue . step
    step k E.EOF = E.yield (E.Continue k) E.EOF
    step k (E.Chunks []) = E.continue $ step k
    step k (E.Chunks xs) = k (E.Chunks (concatMap f xs)) >>== loop
