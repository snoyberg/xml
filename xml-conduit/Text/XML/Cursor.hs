-- | This module provides for simple DOM traversal. It is inspired by XPath. There are two central concepts here:
--
-- * A 'Cursor' represents a node in the DOM. It also contains information on the node's /location/. While the 'Node' datatype will only know of its children, a @Cursor@ knows about its parent and siblings as well. (The underlying mechanism allowing this is called a zipper, see <http://www.haskell.org/haskellwiki/Zipper> and <http://www.haskell.org/haskellwiki/Tying_the_Knot>.)
--
-- * An 'Axis', in its simplest form, takes a @Cursor@ and returns a list of @Cursor@s. It is used for selections, such as finding children, ancestors, etc. Axes can be chained together to express complex rules, such as all children named /foo/.
--
-- The terminology used in this module is taken directly from the XPath
-- specification: <http://www.w3.org/TR/xpath/>. For those familiar with XPath,
-- the one major difference is that attributes are not considered nodes in this
-- module.
module Text.XML.Cursor
    (
    -- * Data types
      Cursor
    , Axis
    -- * Production
    , fromDocument
    , fromNode
    , cut
    -- * Axes
    , parent
    , CG.precedingSibling
    , CG.followingSibling
    , child
    , node
    , CG.preceding
    , CG.following
    , CG.ancestor
    , descendant
    , orSelf
      -- ** Filters
    , check
    , checkNode
    , checkElement
    , checkName
    , anyElement
    , element
    , laxElement
    , content
    , attribute
    , laxAttribute
    , hasAttribute
    , attributeIs
    -- * Operators
    , (CG.&|)
    , (CG.&/)
    , (CG.&//)
    , (CG.&.//)
    , (CG.$|)
    , (CG.$/)
    , (CG.$//)
    , (CG.$.//)
    , (CG.>=>)
    -- * Type classes
    , Boolean(..)
    -- * Error handling
    , force
    , forceM
    ) where

import           Control.Monad
import           Data.Function                (on)
import           Text.XML
import qualified Control.Failure              as F
import qualified Data.Text                    as T
import qualified Data.Map                     as Map
import qualified Text.XML.Cursor.Generic      as CG
import           Text.XML.Cursor.Generic      (node, child, parent, descendant, orSelf)
import           Data.Maybe                   (maybeToList)

-- TODO: Consider [Cursor] -> [Cursor]?
-- | The type of an Axis that returns a list of Cursors.
-- They are roughly modeled after <http://www.w3.org/TR/xpath/#axes>.
-- 
-- Axes can be composed with '>=>', where e.g. @f >=> g@ means that on all results of
-- the @f@ axis, the @g@ axis will be applied, and all results joined together. 
-- Because Axis is just a type synonym for @Cursor -> [Cursor]@, it is possible to use
-- other standard functions like '>>=' or 'concatMap' similarly.
-- 
-- The operators '&|', '&/', '&//' and '&.//' can be used to combine axes so that the second
-- axis works on the context nodes, children, descendants, respectively the context node as 
-- well as its descendants of the results of the first axis.
-- 
-- The operators '$|', '$/', '$//' and '$.//' can be used to apply an axis (right-hand side)
-- to a cursor so that it is applied on the cursor itself, its children, its descendants,
-- respectively itself and its descendants.
-- 
-- Note that many of these operators also work on /generalised Axes/ that can return 
-- lists of something other than Cursors, for example Content elements.
type Axis = Cursor -> [Cursor]

-- XPath axes as in http://www.w3.org/TR/xpath/#axes

-- TODO: Decide whether to use an existing package for this
-- | Something that can be used in a predicate check as a boolean.
class Boolean a where
    bool :: a -> Bool

instance Boolean Bool where 
    bool = id
instance Boolean [a] where 
    bool = not . null
instance Boolean (Maybe a) where 
    bool (Just _) = True
    bool _        = False
instance Boolean (Either a b) where
    bool (Left _)  = False
    bool (Right _) = True

-- | A cursor: contains an XML 'Node' and pointers to its children, ancestors and siblings.
type Cursor = CG.Cursor Node

-- | Cut a cursor off from its parent. The idea is to allow restricting the scope of queries on it.
cut :: Cursor -> Cursor
cut = fromNode . CG.node

-- | Convert a 'Document' to a 'Cursor'. It will point to the document root.
fromDocument :: Document -> Cursor
fromDocument = fromNode . NodeElement . documentRoot

-- | Convert a 'Node' to a 'Cursor' (without parents).
fromNode :: Node -> Cursor
fromNode =
    CG.toCursor cs
  where
    cs (NodeElement (Element _ _ x)) = x
    cs _ = []

-- | Filter cursors that don't pass a check.
check :: Boolean b => (Cursor -> b) -> Axis
check f c = case bool $ f c of
              False -> []
              True -> [c]

-- | Filter nodes that don't pass a check.
checkNode :: Boolean b => (Node -> b) -> Axis
checkNode f c = check (f . node) c

-- | Filter elements that don't pass a check, and remove all non-elements.
checkElement :: Boolean b => (Element -> b) -> Axis
checkElement f c = case node c of
                     NodeElement e -> case bool $ f e of
                                        True -> [c]
                                        False -> []
                     _ -> []

-- | Filter elements that don't pass a name check, and remove all non-elements.
checkName :: Boolean b => (Name -> b) -> Axis
checkName f c = checkElement (f . elementName) c

-- | Remove all non-elements. Compare roughly to XPath:
-- /A node test * is true for any node of the principal node type. For example, child::* will select all element children of the context node [...]/.
anyElement :: Axis
anyElement = checkElement (const True)

-- | Select only those elements with a matching tag name. XPath:
-- /A node test that is a QName is true if and only if the type of the node (see [5 Data Model]) is the principal node type and has an expanded-name equal to the expanded-name specified by the QName./
element :: Name -> Axis
element n = checkName (== n)

-- | Select only those elements with a loosely matching tag name. Namespace and case are ignored. XPath:
-- /A node test that is a QName is true if and only if the type of the node (see [5 Data Model]) is the principal node type and has an expanded-name equal to the expanded-name specified by the QName./
laxElement :: T.Text -> Axis
laxElement n = checkName (on (==) T.toCaseFold n . nameLocalName)

-- | Select only text nodes, and directly give the 'Content' values. XPath:
-- /The node test text() is true for any text node./
-- 
-- Note that this is not strictly an 'Axis', but will work with most combinators.
content :: Cursor -> [T.Text]
content c = case node c of
              (NodeContent v) -> [v]
              _               -> []

-- | Select attributes on the current element (or nothing if it is not an element). XPath:
-- /the attribute axis contains the attributes of the context node; the axis will be empty unless the context node is an element/
-- 
-- Note that this is not strictly an 'Axis', but will work with most combinators.
-- 
-- The return list of the generalised axis contains as elements lists of 'Content' 
-- elements, each full list representing an attribute value.
attribute :: Name -> Cursor -> [T.Text]
attribute n c =
    case node c of
        NodeElement e -> maybeToList $ Map.lookup n $ elementAttributes e
        _ -> []

-- | Select attributes on the current element (or nothing if it is not an element).  Namespace and case are ignored. XPath:
-- /the attribute axis contains the attributes of the context node; the axis will be empty unless the context node is an element/
-- 
-- Note that this is not strictly an 'Axis', but will work with most combinators.
-- 
-- The return list of the generalised axis contains as elements lists of 'Content' 
-- elements, each full list representing an attribute value.
laxAttribute :: T.Text -> Cursor -> [T.Text]
laxAttribute n c =
    case node c of
        NodeElement e -> do
            (n', v) <- Map.toList $ elementAttributes e
            guard $ (on (==) T.toCaseFold) n (nameLocalName n')
            return v
        _ -> []

-- | Select only those element nodes with the given attribute.
hasAttribute :: Name -> Axis
hasAttribute n c =
    case node c of
        NodeElement (Element _ as _) -> maybe [] (const [c]) $ Map.lookup n as
        _ -> []

-- | Select only those element nodes containing the given attribute key/value pair.
attributeIs :: Name -> T.Text -> Axis
attributeIs n v c =
    case node c of
        NodeElement (Element _ as _) -> if Just v == Map.lookup n as then [c] else []
        _ -> []

force :: F.Failure e f => e -> [a] -> f a
force e [] = F.failure e
force _ (x:_) = return x

forceM :: F.Failure e f => e -> [f a] -> f a
forceM e [] = F.failure e
forceM _ (x:_) = x
