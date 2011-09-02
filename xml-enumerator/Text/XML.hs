{-# LANGUAGE DeriveDataTypeable #-}
-- | DOM-based parsing and rendering.
--
-- Unlike the "Text.XML.Enumerator.Document" module, the functions and
-- datatypes here require that all entities are resolved at parsing. Unresolved
-- entities are considered a parse error. If you need to allow entity
-- passthrough, please use the above-mentioned module. This module is
-- recommended for most use cases.
--
-- While many of the datatypes in this module are simply re-exported from
-- "Data.XML.Types", 'Document', 'Node' and 'Element' are all redefined here to
-- disallow the possibility of unresolved entities. Conversion functions are
-- provided to switch between the two sets of datatypes.
--
-- For simpler, bidirectional traversal of the DOM tree, see the
-- "Text.XML.Enumerator.Cursor" module.
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
    , readFile
    , readFile_
    , parseLBS
    , parseLBS_
    , parseEnum
    , parseEnum_
    , fromEvents
    , UnresolvedEntityException (..)
      -- ** Settings
    , ParseSettings
    , def
    , psDecodeEntities
      -- * Rendering
    , writeFile
    , writePrettyFile
    , renderLBS
    , prettyLBS
    , renderText
    , prettyText
    , renderBytes
    , prettyBytes
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

readFile :: FilePath -> ParseSettings -> IO (Either SomeException Document)
readFile fn = parseEnum $ enumFile fn

readFile_ :: FilePath -> ParseSettings -> IO Document
readFile_ fn = parseEnum_ $ enumFile fn

lbsEnum :: Monad m => L.ByteString -> Enumerator ByteString m a
lbsEnum = enumList 8 . L.toChunks

parseLBS :: L.ByteString -> ParseSettings -> Either SomeException Document
parseLBS lbs = runIdentity . parseEnum (lbsEnum lbs)

parseLBS_ :: L.ByteString -> ParseSettings -> Document
parseLBS_ lbs = runIdentity . parseEnum_ (lbsEnum lbs)

parseEnum :: Monad m
          => Enumerator ByteString m Document
          -> ParseSettings
          -> m (Either SomeException Document)
parseEnum enum de = run $ enum $$ joinI $ P.parseBytes de $$ fromEvents

parseEnum_ :: Monad m
           => Enumerator ByteString m Document
           -> ParseSettings
           -> m Document
parseEnum_ enum de = run_ $ enum $$ joinI $ P.parseBytes de $$ fromEvents

fromEvents :: Monad m => Iteratee X.Event m Document
fromEvents = do
    d <- D.fromEvents
    either (throwError . UnresolvedEntityException) return $ fromXMLDocument d

data UnresolvedEntityException = UnresolvedEntityException (Set Text)
    deriving (Show, Typeable)
instance Exception UnresolvedEntityException

renderBytes :: MonadIO m => Document -> Enumerator ByteString m a
renderBytes doc = enumList 8 (D.toEvents $ toXMLDocument doc) `joinE` R.renderBytes

prettyBytes :: MonadIO m => Document -> Enumerator ByteString m a
prettyBytes doc = enumList 8 (D.toEvents $ toXMLDocument doc) `joinE` R.prettyBytes

writeFile :: FilePath -> Document -> IO ()
writeFile fn doc = SIO.withBinaryFile fn SIO.WriteMode $ \h ->
    run_ $ renderBytes doc $$ iterHandle h

-- | Pretty prints via 'prettyBytes'.
writePrettyFile :: FilePath -> Document -> IO ()
writePrettyFile fn doc = SIO.withBinaryFile fn SIO.WriteMode $ \h ->
    run_ $ prettyBytes doc $$ iterHandle h

renderLBS :: Document -> L.ByteString
renderLBS doc =
    L.fromChunks $ unsafePerformIO $ lazyConsume $ renderBytes doc

-- | Pretty prints via 'prettyBytes'.
prettyLBS :: Document -> L.ByteString
prettyLBS doc =
    L.fromChunks $ unsafePerformIO $ lazyConsume $ prettyBytes doc

renderText :: Document -> TL.Text
renderText = TLE.decodeUtf8 . renderLBS

-- | Pretty prints via 'prettyBytes'.
prettyText :: Document -> TL.Text
prettyText = TLE.decodeUtf8 . prettyLBS
