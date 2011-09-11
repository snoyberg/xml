{-# LANGUAGE DeriveDataTypeable #-}
-- | DOM-based parsing and rendering.
--
-- This module requires that all entities be resolved at parsing. If you need
-- to interact with unresolved entities, please use "Text.XML.Unresolved". This
-- is the recommended module for most uses cases.
--
-- While many of the datatypes in this module are simply re-exported from
-- @Data.XML.Types@, 'Document', 'Node' and 'Element' are all redefined here to
-- disallow the possibility of unresolved entities. Conversion functions are
-- provided to switch between the two sets of datatypes.
--
-- For simpler, bidirectional traversal of the DOM tree, see the
-- "Text.XML.Cursor" module.
module Text.XML
    ( -- * Data types
      Document (..)
    , Prologue (..)
    , Instruction (..)
    , Miscellaneous (..)
    , Node (..)
    , Element (..)
    , Name (..)
    , Doctype (..)
    , ExternalID (..)
      -- * Parsing
      -- ** Files
    , readFile
    , readFile_
      -- ** Bytes
    , parseLBS
    , parseLBS_
    , parseEnum
    , parseEnum_
      -- ** Text
    , parseText
    , parseText_
    , parseTextEnum
    , parseTextEnum_
      -- ** Other
    , fromEvents
    , UnresolvedEntityException (..)
      -- * Rendering
    , writeFile
    , renderLBS
    , renderText
    , renderBytes
      -- * Settings
    , def
      -- ** Parsing
    , ParseSettings
    , psDecodeEntities
      -- ** Rendering
    , R.RenderSettings
    , R.rsPretty
      -- * Conversion
    , toXMLDocument
    , fromXMLDocument
    , toXMLNode
    , fromXMLNode
    , toXMLElement
    , fromXMLElement
    ) where

import qualified Data.XML.Types as X
import Data.XML.Types
    ( Prologue (..)
    , Miscellaneous (..)
    , Instruction (..)
    , Name (..)
    , Doctype (..)
    , ExternalID (..)
    )
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Text.XML.Stream.Parse as P
import qualified Text.XML.Unresolved as D
import qualified Text.XML.Stream.Render as R
import qualified Data.Text as T
import Data.Either (partitionEithers)
import Prelude hiding (readFile, writeFile)
import Control.Exception (SomeException, Exception)
import Data.Enumerator.Binary (enumFile, iterHandle)
import Control.Monad.IO.Class (MonadIO)
import Text.XML.Stream.Parse (ParseSettings, def, psDecodeEntities)
import Data.Enumerator
    ( Enumerator, Iteratee, throwError, ($$), run, run_, joinI, enumList
    , joinE
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Functor.Identity (runIdentity)
import qualified System.IO as SIO
import System.IO.Unsafe (unsafePerformIO)
import Text.XML.Unresolved (lazyConsume)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

data Document = Document
    { documentPrologue :: Prologue
    , documentRoot :: Element
    , documentEpilogue :: [Miscellaneous]
    }
  deriving (Show, Eq, Typeable)

data Node
    = NodeElement Element
    | NodeInstruction Instruction
    | NodeContent Text
    | NodeComment Text
  deriving (Show, Eq, Typeable)

data Element = Element
    { elementName :: Name
    , elementAttributes :: [(Name, Text)]
    , elementNodes :: [Node]
    }
  deriving (Show, Eq, Typeable)

{-
readFile :: FilePath -> ParseSettings -> IO (Either SomeException Document)
readFile_ :: FIlePath -> ParseSettings -> IO Document
-}

toXMLDocument :: Document -> X.Document
toXMLDocument (Document a b c) = X.Document a (toXMLElement b) c

toXMLElement :: Element -> X.Element
toXMLElement (Element name as nodes) =
    X.Element name as' nodes'
  where
    as' = map (\(x, y) -> (x, [X.ContentText y])) as
    nodes' = map toXMLNode nodes

toXMLNode :: Node -> X.Node
toXMLNode (NodeElement e) = X.NodeElement $ toXMLElement e
toXMLNode (NodeContent t) = X.NodeContent $ X.ContentText t
toXMLNode (NodeComment c) = X.NodeComment c
toXMLNode (NodeInstruction i) = X.NodeInstruction i

fromXMLDocument :: X.Document -> Either (Set Text) Document
fromXMLDocument (X.Document a b c) =
    case fromXMLElement b of
        Left es -> Left es
        Right b' -> Right $ Document a b' c

fromXMLElement :: X.Element -> Either (Set Text) Element
fromXMLElement (X.Element name as nodes) =
    case (lnodes, las) of
        ([], []) -> Right $ Element name ras rnodes
        (x, []) -> Left $ Set.unions x
        ([], y) -> Left $ Set.unions y
        (x, y) -> Left $ Set.unions x `Set.union` Set.unions y
  where
    enodes = map fromXMLNode nodes
    (lnodes, rnodes) = partitionEithers enodes
    eas = map go as
    (las, ras) = partitionEithers eas
    go (x, y) =
        case go' [] id y of
            Left es -> Left es
            Right y' -> Right (x, y')
    go' [] front [] = Right $ T.concat $ front []
    go' errs _ [] = Left $ Set.fromList errs
    go' errs front (X.ContentText t:ys) = go' errs (front . (:) t) ys
    go' errs front (X.ContentEntity t:ys) = go' (t : errs) front ys

fromXMLNode :: X.Node -> Either (Set Text) Node
fromXMLNode (X.NodeElement e) =
    either Left (Right . NodeElement) $ fromXMLElement e
fromXMLNode (X.NodeContent (X.ContentText t)) = Right $ NodeContent t
fromXMLNode (X.NodeContent (X.ContentEntity t)) = Left $ Set.singleton t
fromXMLNode (X.NodeComment c) = Right $ NodeComment c
fromXMLNode (X.NodeInstruction i) = Right $ NodeInstruction i

readFile :: ParseSettings -> FilePath -> IO (Either SomeException Document)
readFile ps fn = parseEnum ps $ enumFile fn

readFile_ :: ParseSettings -> FilePath -> IO Document
readFile_ ps fn = parseEnum_ ps $ enumFile fn

lbsEnum :: Monad m => L.ByteString -> Enumerator ByteString m a
lbsEnum = enumList 8 . L.toChunks

textEnum :: Monad m => TL.Text -> Enumerator Text m a
textEnum = enumList 8 . TL.toChunks

parseLBS :: ParseSettings -> L.ByteString -> Either SomeException Document
parseLBS ps = runIdentity . parseEnum ps . lbsEnum

parseLBS_ :: ParseSettings -> L.ByteString -> Document
parseLBS_ ps = runIdentity . parseEnum_ ps . lbsEnum

parseEnum :: Monad m
          => ParseSettings
          -> Enumerator ByteString m Document
          -> m (Either SomeException Document)
parseEnum de enum = run $ enum $$ joinI $ P.parseBytes de $$ fromEvents

parseEnum_ :: Monad m
           => ParseSettings
           -> Enumerator ByteString m Document
           -> m Document
parseEnum_ de enum = run_ $ enum $$ joinI $ P.parseBytes de $$ fromEvents

parseText :: ParseSettings -> TL.Text -> Either SomeException Document
parseText ps = runIdentity . parseTextEnum ps . textEnum

parseText_ :: ParseSettings -> TL.Text -> Document
parseText_ ps = runIdentity . parseTextEnum_ ps . textEnum

parseTextEnum :: Monad m
              => ParseSettings
              -> Enumerator Text m Document
              -> m (Either SomeException Document)
parseTextEnum de enum = run $ enum $$ joinI $ P.parseText de $$ fromEvents

parseTextEnum_ :: Monad m
               => ParseSettings
               -> Enumerator Text m Document
               -> m Document
parseTextEnum_ de enum = run_ $ enum $$ joinI $ P.parseText de $$ fromEvents

fromEvents :: Monad m => Iteratee X.Event m Document
fromEvents = do
    d <- D.fromEvents
    either (throwError . UnresolvedEntityException) return $ fromXMLDocument d

data UnresolvedEntityException = UnresolvedEntityException (Set Text)
    deriving (Show, Typeable)
instance Exception UnresolvedEntityException

renderBytes :: MonadIO m => R.RenderSettings -> Document -> Enumerator ByteString m a
renderBytes rs doc = enumList 8 (D.toEvents $ toXMLDocument doc) `joinE` R.renderBytes rs

writeFile :: R.RenderSettings -> FilePath -> Document -> IO ()
writeFile rs fn doc = SIO.withBinaryFile fn SIO.WriteMode $ \h ->
    run_ $ renderBytes rs doc $$ iterHandle h

renderLBS :: R.RenderSettings -> Document -> L.ByteString
renderLBS rs doc =
    L.fromChunks $ unsafePerformIO $ lazyConsume $ renderBytes rs doc

renderText :: R.RenderSettings -> Document -> TL.Text
renderText rs = TLE.decodeUtf8 . renderLBS rs
