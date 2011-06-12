{-# LANGUAGE DeriveDataTypeable #-}
module Text.XML.Enumerator.Resolved
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
    , DecodeEntities
    , decodeEntities
    , readFile
    , readFile_
    , parseLBS
    , parseLBS_
    , parseEnum
    , parseEnum_
    , fromEvents
    , UnresolvedEntityException
      -- * Rendering
    , writeFile
    , writePrettyFile
    , renderLBS
    , prettyLBS
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
import Text.XML.Enumerator.Parse (DecodeEntities, decodeEntities)
import qualified Text.XML.Enumerator.Parse as P
import qualified Text.XML.Enumerator.Document as D
import qualified Text.XML.Enumerator.Render as R
import qualified Data.Text as T
import Data.Either (partitionEithers)
import Prelude hiding (readFile, writeFile)
import Control.Exception (SomeException, Exception)
import Data.Enumerator.Binary (enumFile, iterHandle)
import Control.Monad.IO.Class (MonadIO)
import Data.Enumerator
    ( Enumerator, Iteratee, throwError, ($$), run, run_, joinI, enumList
    , joinE
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Functor.Identity (runIdentity)
import qualified System.IO as SIO
import System.IO.Unsafe (unsafePerformIO)
import Text.XML.Enumerator.Document (lazyConsume)

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
readFile :: FilePath -> DecodeEntities -> IO (Either SomeException Document)
readFile_ :: FIlePath -> DecodeEntities -> IO Document
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

fromXMLDocument :: X.Document -> Either [Text] Document
fromXMLDocument (X.Document a b c) =
    case fromXMLElement b of
        Left es -> Left es
        Right b' -> Right $ Document a b' c

fromXMLElement :: X.Element -> Either [Text] Element
fromXMLElement (X.Element name as nodes) =
    case (lnodes, las) of
        ([], []) -> Right $ Element name ras rnodes
        (x, []) -> Left $ concat x
        ([], y) -> Left $ concat y
        (x, y) -> Left $ concat $ x ++ y
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
    go' errs _ [] = Left errs
    go' errs front (X.ContentText t:ys) = go' errs (front . (:) t) ys
    go' errs front (X.ContentEntity t:ys) = go' (t : errs) front ys

fromXMLNode :: X.Node -> Either [Text] Node
fromXMLNode (X.NodeElement e) =
    either Left (Right . NodeElement) $ fromXMLElement e
fromXMLNode (X.NodeContent (X.ContentText t)) = Right $ NodeContent t
fromXMLNode (X.NodeContent (X.ContentEntity t)) = Left [t]
fromXMLNode (X.NodeComment c) = Right $ NodeComment c
fromXMLNode (X.NodeInstruction i) = Right $ NodeInstruction i

readFile :: FilePath -> DecodeEntities -> IO (Either SomeException Document)
readFile fn = parseEnum $ enumFile fn

readFile_ :: FilePath -> DecodeEntities -> IO Document
readFile_ fn = parseEnum_ $ enumFile fn

lbsEnum :: Monad m => L.ByteString -> Enumerator ByteString m a
lbsEnum = enumList 8 . L.toChunks

parseLBS :: L.ByteString -> DecodeEntities -> Either SomeException Document
parseLBS lbs = runIdentity . parseEnum (lbsEnum lbs)

parseLBS_ :: L.ByteString -> DecodeEntities -> Document
parseLBS_ lbs = runIdentity . parseEnum_ (lbsEnum lbs)

parseEnum :: Monad m
          => Enumerator ByteString m Document
          -> DecodeEntities
          -> m (Either SomeException Document)
parseEnum enum de = run $ enum $$ joinI $ P.parseBytes de $$ fromEvents

parseEnum_ :: Monad m
           => Enumerator ByteString m Document
           -> DecodeEntities
           -> m Document
parseEnum_ enum de = run_ $ enum $$ joinI $ P.parseBytes de $$ fromEvents

fromEvents :: Monad m => Iteratee X.Event m Document
fromEvents = do
    d <- D.fromEvents
    either (throwError . UnresolvedEntityException) return $ fromXMLDocument d

data UnresolvedEntityException = UnresolvedEntityException [Text]
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
