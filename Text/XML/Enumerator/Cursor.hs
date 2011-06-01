module Text.XML.Enumerator.Cursor
    ( Cursor
    , fromDocument
    , toCursor
    , cut
    , parent
    , precedingSibling
    , followingSibling
    , child
    , node
    , preceding
    , following
    , ancestor
    , descendant
    , orSelf
    , check
    , predicate
    , checkNode
    , checkElement
    , checkName
    , anyElement
    , element
    , content
    , (>=>)
    ) where

import Data.XML.Types
import Control.Monad
import Data.List (foldl')

-- TODO: Consider [Cursor] -> [Cursor]?
type Axis = Cursor -> [Cursor]

-- XPath axes as in http://www.w3.org/TR/xpath/#axes

type DiffCursor = [Cursor] -> [Cursor]


data Cursor = Cursor
    { parent' :: Maybe Cursor
    , precedingSibling' :: DiffCursor
    , followingSibling' :: DiffCursor
    , child :: [Cursor] -- the child axis contains the children of the context node
    , node :: Node
    }

instance Show Cursor where
    show Cursor { node = n } = "Cursor @ " ++ show n

-- Idea: allow restricting the scope
cut :: Cursor -> Cursor
cut c = c { parent' = Nothing }

-- the parent axis contains the parent of the context node, if there is one
parent :: Axis
parent c = case parent' c of
             Nothing -> []
             Just p -> [p]

-- the preceding-sibling axis contains all the preceding siblings of the context node; if the context node is an attribute node or namespace node, the preceding-sibling axis is empty
precedingSibling :: Axis
precedingSibling = ($ []) . precedingSibling'

-- the following-sibling axis contains all the following siblings of the context node; if the context node is an attribute node or namespace node, the following-sibling axis is empty
followingSibling :: Axis
followingSibling = ($ []) . followingSibling'

fromDocument :: Document -> Cursor
fromDocument = toCursor . NodeElement . documentRoot

toCursor :: Node -> Cursor
toCursor = toCursor' Nothing id id

toCursor' :: Maybe Cursor -> DiffCursor -> DiffCursor -> Node -> Cursor
toCursor' par pre fol n =
    me
  where
    me = Cursor par pre fol chi n
    chi' =
        case n of
            NodeElement (Element _ _ x) -> x
            _ -> []
    chi = go id chi' []
    go _ [] = id
    go pre' (n':ns') =
        (:) me' . fol'
      where
        me' = toCursor' (Just me) pre' fol' n'
        fol' = go (pre' . (:) me') ns'

-- the preceding axis contains all nodes in the same document as the context node that are before the context node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes
preceding :: Axis
preceding c =
    go (precedingSibling' c []) (parent c >>= preceding)
  where
    go x y = foldl' (\b a -> go' a b) y x
    go' :: Cursor -> DiffCursor
    go' x rest = foldl' (\b a -> go' a b) (x : rest) (child x)

-- the following axis contains all nodes in the same document as the context node that are after the context node in document order, excluding any descendants and excluding attribute nodes and namespace nodes
following :: Axis
following c =
    go (followingSibling' c) (parent c >>= following)
  where
    go x z =
        foldr (\a b -> go' a b) z y
      where
        y = x []
    go' :: Cursor -> DiffCursor
    go' x rest = x : foldr (\a b -> go' a b) rest (child x)

-- the ancestor axis contains the ancestors of the context node; the ancestors of the context node consist of the parent of context node and the parent's parent and so on; thus, the ancestor axis will always include the root node, unless the context node is the root node
ancestor :: Axis
ancestor = parent >=> (\p -> p : ancestor p)

-- the descendant axis contains the descendants of the context node; a descendant is a child or a child of a child and so on; thus the descendant axis never contains attribute or namespace nodes
descendant :: Axis
descendant = child >=> (\c -> c : descendant c)

orSelf :: Axis -> Axis
orSelf ax c = c : ax c

check :: (Cursor -> Bool) -> Axis
check f c = case f c of
              False -> []
              True -> [c]

predicate :: (Cursor -> [a]) -> Axis
predicate f = check (not . null . f)

checkNode :: (Node -> Bool) -> Axis
checkNode f c = check (f . node) c

checkElement :: (Element -> Bool) -> Axis
checkElement f c = case node c of
                     NodeElement e -> case f e of
                                        True -> [c]
                                        False -> []
                     _ -> []

checkName :: (Name -> Bool) -> Axis
checkName f c = checkElement (f . elementName) c

anyElement :: Axis
anyElement = checkElement (const True)

element :: Name -> Axis
element n = checkName (== n)

content :: Cursor -> [Content]
content = orSelf descendant >=> f . node
    where f (NodeContent c) = [c]
          f _               = []
