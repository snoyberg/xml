module Text.XML.Enumerator.Cursor
    ( Cursor
    , toCursor
    , parent
    , precedingSibling
    , followingSibling
    , children
    , node
    {-
    , preceding
    , following
    -}
    , ancestor
--    , descendant
    ) where

import Data.XML.Types

type DiffCursor = [Cursor] -> [Cursor]

data Cursor = Cursor
    { parent :: Maybe Cursor
    , precedingSibling' :: DiffCursor
    , followingSibling' :: DiffCursor
    , children :: [Cursor]
    , node :: Node
    }

precedingSibling :: Cursor -> [Cursor]
precedingSibling = ($ []) . precedingSibling'

followingSibling :: Cursor -> [Cursor]
followingSibling = ($ []) . followingSibling'

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
        fol' = go ((:) me' . pre') ns'

{- FIXME
preceding :: Cursor -> [Cursor]
preceding c =
    precedingSibling' c
        $ case parent c of
            Nothing -> []
            Just p -> p : preceding p

following :: Cursor -> [Cursor]
following
-}

ancestor :: Cursor -> [Cursor]
ancestor c =
    case parent c of
        Nothing -> []
        Just p -> p : ancestor p

{-
descendant :: Cursor -> [Cursor]
descendant =
    go' . map go . children
  where
    go :: Cursor -> DiffCursor
    go c = (c :) . go' . map go (children c)
    go' :: [DiffCursor] -> DiffCursor
    go' = undefined
-}
