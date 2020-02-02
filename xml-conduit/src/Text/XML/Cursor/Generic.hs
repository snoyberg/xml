-- | Generalized cursors to be applied to different nodes.
module Text.XML.Cursor.Generic
    ( -- * Core
      Cursor
    , Axis
    , toCursor
    , node
      -- * Axes
    , child
    , parent
    , precedingSibling
    , followingSibling
    , ancestor
    , descendant
    , orSelf
    , preceding
    , following
      -- * Operators
    , (&|)
    , (&/)
    , (&//)
    , (&.//)
    , ($|)
    , ($/)
    , ($//)
    , ($.//)
    , (>=>)
    ) where

import Data.Maybe (maybeToList)
import Data.List (foldl')
import Control.Monad ((>=>))

type DiffCursor node = [Cursor node] -> [Cursor node]
type Axis node = Cursor node -> [Cursor node]

-- | A cursor: contains an XML 'Node' and pointers to its children, ancestors and siblings.
data Cursor node = Cursor
    { parent' :: Maybe (Cursor node)
    , precedingSibling' :: DiffCursor node
    , followingSibling' :: DiffCursor node
    -- | The child axis. XPath:
    -- /the child axis contains the children of the context node/.
    , child :: [Cursor node]
    -- | The current node.
    , node :: node
    }

instance Show node => Show (Cursor node) where
    show Cursor { node = n } = "Cursor @ " ++ show n

toCursor :: (node -> [node]) -- ^ get children
         -> node
         -> Cursor node
toCursor cs = toCursor' cs Nothing id id

toCursor' :: (node -> [node])
          -> Maybe (Cursor node) -> DiffCursor node -> DiffCursor node -> node -> Cursor node
toCursor' cs par pre fol n =
    me
  where
    me = Cursor par pre fol chi n
    chi' = cs n
    chi = go id chi' []
    go _ [] = id
    go pre' (n':ns') =
        (:) me' . fol'
      where
        me' = toCursor' cs (Just me) pre' fol' n'
        fol' = go (pre' . (:) me') ns'

-- | The parent axis. As described in XPath:
-- /the parent axis contains the parent of the context node, if there is one/.
--
-- Every node but the root element of the document has a parent. Parent nodes
-- will always be 'NodeElement's.
parent :: Axis node
parent = maybeToList . parent'

-- | The preceding-sibling axis. XPath:
-- /the preceding-sibling axis contains all the preceding siblings of the context node [...]/.
precedingSibling :: Axis node
precedingSibling = ($ []) . precedingSibling'

-- | The following-sibling axis. XPath:
-- /the following-sibling axis contains all the following siblings of the context node [...]/.
followingSibling :: Axis node
followingSibling = ($ []) . followingSibling'

-- | The preceding axis. XPath:
-- /the preceding axis contains all nodes in the same document as the context node that are before the context node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes/.
preceding :: Axis node
preceding c =
    go (precedingSibling' c []) (parent c >>= preceding)
  where
    go x y = foldl' (flip go') y x
    go' x rest = foldl' (flip  go') (x : rest) (child x)

-- | The following axis. XPath:
-- /the following axis contains all nodes in the same document as the context node that are after the context node in document order, excluding any descendants and excluding attribute nodes and namespace nodes/.
following :: Axis node
following c =
    go (followingSibling' c) (parent c >>= following)
  where
    go x z = foldr go' z (x [])
    go' x rest = x : foldr go' rest (child x)

-- | The ancestor axis. XPath:
-- /the ancestor axis contains the ancestors of the context node; the ancestors of the context node consist of the parent of context node and the parent's parent and so on; thus, the ancestor axis will always include the root node, unless the context node is the root node/.
ancestor :: Axis node
ancestor = parent >=> (\p -> p : ancestor p)

-- | The descendant axis. XPath:
-- /the descendant axis contains the descendants of the context node; a descendant is a child or a child of a child and so on; thus the descendant axis never contains attribute or namespace nodes/.
descendant :: Axis node
descendant = child >=> (\c -> c : descendant c)

-- | Modify an axis by adding the context node itself as the first element of the result list.
orSelf :: Axis node -> Axis node
orSelf ax c = c : ax c

infixr 1 &|
infixr 1 &/ 
infixr 1 &// 
infixr 1 &.// 
infixr 1 $|
infixr 1 $/
infixr 1 $//
infixr 1 $.//

-- | Apply a function to the result of an axis.
(&|) :: (Cursor node -> [a]) -> (a -> b) -> (Cursor node -> [b])
f &| g = map g . f

-- | Combine two axes so that the second works on the children of the results
-- of the first.
(&/) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
f &/ g = f >=> child >=> g

-- | Combine two axes so that the second works on the descendants of the results
-- of the first.
(&//) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
f &// g = f >=> descendant >=> g

-- | Combine two axes so that the second works on both the result nodes, and their
-- descendants.
(&.//) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
f &.// g = f >=> orSelf descendant >=> g

-- | Apply an axis to a 'Cursor node'.
($|) :: Cursor node -> (Cursor node -> a) -> a
v $| f = f v

-- | Apply an axis to the children of a 'Cursor node'.
($/) :: Cursor node -> (Cursor node -> [a]) -> [a]
v $/ f = child v >>= f

-- | Apply an axis to the descendants of a 'Cursor node'.
($//) :: Cursor node -> (Cursor node -> [a]) -> [a]
v $// f = descendant v >>= f

-- | Apply an axis to a 'Cursor node' as well as its descendants.
($.//) :: Cursor node -> (Cursor node -> [a]) -> [a]
v $.// f = orSelf descendant v >>= f
