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

instance Show Cursor where
    show Cursor { node = n } = "Cursor @ " ++ show n

precedingSibling :: Cursor -> [Cursor]
precedingSibling = reverse . ($ []) . precedingSibling'

followingSibling :: Cursor -> [Cursor]
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

preceding' :: Cursor -> [Cursor]
preceding' c = precedingSibling' c $ case parent c of Nothing -> []; Just p -> preceding p

preceding :: Cursor -> [Cursor]
preceding = reverse . preceding'

following :: Cursor -> [Cursor]
following c = followingSibling' c $ case parent c of Nothing -> []; Just p -> following p

ancestor :: Cursor -> [Cursor]
ancestor c =
    case parent c of
        Nothing -> []
        Just p -> p : ancestor p

descendant :: Cursor -> [Cursor]
descendant = concatMap (\c -> c : descendant c) . children
