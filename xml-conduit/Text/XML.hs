{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
      -- ** Bytes
    , parseLBS
    , parseLBS_
    , sinkDoc
      -- ** Text
    , parseText
    , parseText_
    , sinkTextDoc
      -- ** Other
    , fromEvents
    , UnresolvedEntityException (..)
    , XMLException (..)
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
import Prelude hiding (readFile, writeFile, FilePath)
import Filesystem.Path.CurrentOS (FilePath, encodeString)
import Control.Exception (SomeException, Exception, throwIO, handle)
import Text.XML.Stream.Parse (ParseSettings, def, psDecodeEntities)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Control.Monad.ST (runST)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (throw)
import Control.Monad.Trans.Resource (MonadUnsafeIO, runExceptionT)
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Lazy (lazyConsume)

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
  deriving (Show, Eq, Ord, Typeable)

data Element = Element
    { elementName :: Name
    , elementAttributes :: [(Name, Text)]
    , elementNodes :: [Node]
    }
  deriving (Show, Eq, Ord, Typeable)

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

readFile :: ParseSettings -> FilePath -> IO Document
readFile ps fp = handle
    (throwIO . InvalidXMLFile fp)
    (C.runResourceT $ CB.sourceFile (encodeString fp) C.$$ sinkDoc ps)

data XMLException = InvalidXMLFile FilePath SomeException
    deriving Typeable

instance Show XMLException where
    show (InvalidXMLFile fp e) = concat
        [ "Error parsing XML file "
        , encodeString fp
        , ": "
        , show e
        ]
instance Exception XMLException

parseLBS :: ParseSettings -> L.ByteString -> Either SomeException Document
parseLBS ps lbs = runST
                $ runExceptionT
                $ CL.sourceList (L.toChunks lbs)
           C.$$ sinkDoc ps

parseLBS_ :: ParseSettings -> L.ByteString -> Document
parseLBS_ ps = either throw id . parseLBS ps

sinkDoc :: C.MonadThrow m
        => ParseSettings
        -> C.Sink ByteString m Document
sinkDoc ps = P.parseBytes ps C.=$ fromEvents

parseText :: ParseSettings -> TL.Text -> Either SomeException Document
parseText ps tl = runST
                $ runExceptionT
                $ CL.sourceList (TL.toChunks tl)
           C.$$ sinkTextDoc ps

parseText_ :: ParseSettings -> TL.Text -> Document
parseText_ ps = either throw id . parseText ps

sinkTextDoc :: C.MonadThrow m
            => ParseSettings
            -> C.Sink Text m Document
sinkTextDoc ps = P.parseText ps C.=$ fromEvents

fromEvents :: C.MonadThrow m => C.Sink X.Event m Document
fromEvents = do
    d <- D.fromEvents
    either (lift . C.monadThrow . UnresolvedEntityException) return $ fromXMLDocument d

data UnresolvedEntityException = UnresolvedEntityException (Set Text)
    deriving (Show, Typeable)
instance Exception UnresolvedEntityException

renderBytes :: MonadUnsafeIO m => R.RenderSettings -> Document -> C.Source m ByteString
renderBytes rs doc = D.renderBytes rs $ toXMLDocument doc

writeFile :: R.RenderSettings -> FilePath -> Document -> IO ()
writeFile rs fp doc =
    C.runResourceT $ renderBytes rs doc C.$$ CB.sinkFile (encodeString fp)

renderLBS :: R.RenderSettings -> Document -> L.ByteString
renderLBS rs doc =
    L.fromChunks $ unsafePerformIO
                 -- not generally safe, but we know that runResourceT
                 -- will not deallocate any of the resources being used
                 -- by the process
                 $ lazyConsume
                 $ renderBytes rs doc

renderText :: R.RenderSettings -> Document -> TL.Text
renderText rs = TLE.decodeUtf8 . renderLBS rs
