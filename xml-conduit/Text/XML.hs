{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
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
      -- *** Entity decoding
    , P.decodeXmlEntities
    , P.decodeHtmlEntities
      -- ** Rendering
    , R.RenderSettings
    , R.rsPretty
    , R.rsNamespaces
    , R.rsAttrOrder
    , R.orderAttrs
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
import Data.Data (Data)
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
import qualified Data.Map as Map
import Data.Set (Set)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Conduit hiding (Source, Sink, Conduit)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (throw)
import Control.Monad.Trans.Resource (runExceptionT)
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Lazy (lazyConsume)

import qualified Text.Blaze as B
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html5 as B5
import qualified Text.Blaze.Internal as BI
import Data.Monoid (mempty, mappend)
import Data.String (fromString)
import Data.List (foldl')
import Control.Arrow (first)

data Document = Document
    { documentPrologue :: Prologue
    , documentRoot :: Element
    , documentEpilogue :: [Miscellaneous]
    }
  deriving (Show, Eq, Typeable, Data)

data Node
    = NodeElement Element
    | NodeInstruction Instruction
    | NodeContent Text
    | NodeComment Text
  deriving (Show, Eq, Ord, Typeable, Data)

data Element = Element
    { elementName :: Name
    , elementAttributes :: Map.Map Name Text
    , elementNodes :: [Node]
    }
  deriving (Show, Eq, Ord, Typeable, Data)

{-
readFile :: FilePath -> ParseSettings -> IO (Either SomeException Document)
readFile_ :: FIlePath -> ParseSettings -> IO Document
-}

toXMLDocument :: Document -> X.Document
toXMLDocument = toXMLDocument' def

toXMLDocument' :: R.RenderSettings -> Document -> X.Document
toXMLDocument' rs (Document a b c) = X.Document a (toXMLElement' rs b) c

toXMLElement :: Element -> X.Element
toXMLElement = toXMLElement' def

toXMLElement' :: R.RenderSettings -> Element -> X.Element
toXMLElement' rs (Element name as nodes) =
    X.Element name as' nodes'
  where
    as' = map (\(x, y) -> (x, [X.ContentText y])) $ R.rsAttrOrder rs name as
    nodes' = map (toXMLNode' rs) nodes

toXMLNode :: Node -> X.Node
toXMLNode = toXMLNode' def

toXMLNode' :: R.RenderSettings -> Node -> X.Node
toXMLNode' rs (NodeElement e) = X.NodeElement $ toXMLElement' rs e
toXMLNode' _ (NodeContent t) = X.NodeContent $ X.ContentText t
toXMLNode' _ (NodeComment c) = X.NodeComment c
toXMLNode' _ (NodeInstruction i) = X.NodeInstruction i

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
    (las, ras') = partitionEithers eas
    ras = Map.fromList ras'
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
    (runResourceT $ CB.sourceFile (encodeString fp) $$ sinkDoc ps)

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
           $$ sinkDoc ps

parseLBS_ :: ParseSettings -> L.ByteString -> Document
parseLBS_ ps = either throw id . parseLBS ps

sinkDoc :: MonadThrow m
        => ParseSettings
        -> Pipe l ByteString o u m Document
sinkDoc ps = P.parseBytesPos ps >+> fromEvents

parseText :: ParseSettings -> TL.Text -> Either SomeException Document
parseText ps tl = runST
                $ runExceptionT
                $ CL.sourceList (TL.toChunks tl)
           $$ sinkTextDoc ps

parseText_ :: ParseSettings -> TL.Text -> Document
parseText_ ps = either throw id . parseText ps

sinkTextDoc :: MonadThrow m
            => ParseSettings
            -> Pipe l Text o u m Document
sinkTextDoc ps = P.parseText ps >+> fromEvents

fromEvents :: MonadThrow m => Pipe l P.EventPos o u m Document
fromEvents = do
    d <- D.fromEvents
    either (lift . monadThrow . UnresolvedEntityException) return $ fromXMLDocument d

data UnresolvedEntityException = UnresolvedEntityException (Set Text)
    deriving (Show, Typeable)
instance Exception UnresolvedEntityException

renderBytes :: MonadUnsafeIO m => R.RenderSettings -> Document -> Pipe l i ByteString u m ()
renderBytes rs doc = D.renderBytes rs $ toXMLDocument' rs doc

writeFile :: R.RenderSettings -> FilePath -> Document -> IO ()
writeFile rs fp doc =
    runResourceT $ renderBytes rs doc $$ CB.sinkFile (encodeString fp)

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

instance B.ToMarkup Document where
    toMarkup (Document _ root _) = B5.docType >> B.toMarkup root

-- | Note that the special element name
-- @{http://www.snoyman.com/xml2html}ie-cond@ with the single attribute @cond@
-- is used to indicate an IE conditional comment.
instance B.ToMarkup Element where
    toMarkup (Element "{http://www.snoyman.com/xml2html}ie-cond" attrs children)
      | [("cond", cond)] <- Map.toList attrs =
        B.preEscapedToMarkup ("<!--[if " :: T.Text)
        `mappend` B.preEscapedToMarkup cond
        `mappend` B.preEscapedToMarkup ("]>" :: T.Text)
        `mappend` mapM_ B.toMarkup children
        `mappend` B.preEscapedToMarkup ("<![endif]-->" :: T.Text)

    toMarkup (Element name' attrs children) =
        if isVoid
            then foldl' (B.!) leaf attrs'
            else foldl' (B.!) parent attrs' childrenHtml
      where
        childrenHtml :: B.Html
        childrenHtml =
            case (name `elem` ["style", "script"], children) of
                (True, [NodeContent t]) -> B.preEscapedToMarkup t
                _ -> mapM_ B.toMarkup children

        isVoid = nameLocalName name' `Set.member` voidElems

        parent :: B.Html -> B.Html
        parent = BI.Parent tag open close
        leaf :: B.Html
        leaf = BI.Leaf tag open (fromString " />")

        name = T.unpack $ nameLocalName name'
        tag = fromString name
        open = fromString $ '<' : name
        close = fromString $ concat ["</", name, ">"]

        attrs' :: [B.Attribute]
        attrs' = map goAttr $ map (first nameLocalName) $ Map.toList attrs
        goAttr (key, value) = B.customAttribute (B.textTag key) $ B.toValue value

instance B.ToMarkup Node where
    toMarkup (NodeElement e) = B.toMarkup e
    toMarkup (NodeContent t) = B.toMarkup t
    toMarkup _ = mempty

voidElems :: Set.Set T.Text
voidElems = Set.fromAscList $ T.words $ T.pack "area base br col command embed hr img input keygen link meta param source track wbr"
