{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.XML.XPath (xp) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator (choice)
import Data.Text (Text, pack, unpack)
import Control.Applicative ((<|>))
import Prelude hiding (takeWhile)
import Text.XML.Enumerator.Cursor
    ( Cursor, parent, child, descendant
    , element, anyElement, content
    , node, attribute
    )
import Control.Monad ((>=>), (<=<))
import Data.String (fromString)
import qualified Text.XML.Enumerator.Resolved as X
import qualified Data.Text as T

xp :: QuasiQuoter
xp = QuasiQuoter
    { quoteExp = go . parse xpParse . pack
    }
  where
    go (Done "" x) = lift' x
    go (Done t _) = error $ "Leftover input: " ++ show t
    go (Fail _ _ s) = error s
    go (Partial f) = go $ f T.empty

data XP = XP
    { xpFromRoot :: Bool
    , xpPieces :: [Piece]
    }
  deriving Show

instance Lift XP where
    lift (XP a b) = [|XP $(lift a) $(lift b)|]

data Piece = Piece
    { pieceAxis :: Axis
    , pieceNodeType :: NodeType
    , pieceConds :: [Cond]
    }
  deriving Show

instance Lift Piece where
    lift (Piece a b c) = [|Piece $(lift a) $(lift b) $(lift c)|]

data Axis = Parent | Child | Ancestor | Descendant
  deriving Show

instance Lift Axis where
    lift Parent = [|Parent|]
    lift Child = [|Child|]
    lift Ancestor = [|Ancestor|]
    lift Descendant = [|Descendant|]

data NodeType = AnyNode | AnyElement | Element Text | Text | AnyAttr | Attr Text
  deriving Show

instance Lift NodeType where
    lift AnyNode = [|AnyNode|]
    lift AnyElement = [|AnyElement|]
    lift (Element t) = [|Element $ pack $(lift $ unpack t)|]
    lift Text = [|Text|]
    lift AnyAttr = [|AnyAttr|]
    lift (Attr t) = [|Attr $ pack $(lift $ unpack t)|]

data Cond = AttrEq X.Name T.Text
  deriving Show

instance Lift Cond where
    lift (AttrEq a b) =
        [|AttrEq $(liftName a) $ pack $(lift $ unpack b)|]

liftName :: X.Name -> Q Exp
liftName (X.Name a b c) =
    [|X.Name $(liftT a) $(liftMT b) $(liftMT c)|]

liftT t = [|pack $(lift $ unpack t)|]
liftMT Nothing = [|Nothing|]
liftMT (Just t) = [|Just $ pack $(lift $ unpack t)|]

xpParse :: Parser XP
xpParse = do
    skipSpace
    fromRoot <- (char '/' >> return True) <|> return False
    ps <- pieces
    return $ XP fromRoot ps

pieces :: Parser [Piece]
pieces = do
    p <- piece
    (char '/' >> pieces >>= return . (p:)) <|> return [p]

piece :: Parser Piece
piece = do
    a <- axis <|> return Child
    nt <- nodeType
    conds <- many cond
    return $ Piece a nt conds

axis :: Parser Axis
axis = choice
    [ string "child::" >> return Child
    , string "parent::" >> return Parent
    , string "ancestor::" >> return Ancestor
    , string "descendant::" >> return Descendant
    , string "/" >> return Descendant
    ]

nodeType :: Parser NodeType
nodeType = choice
    [ string "node()" >> return AnyNode
    , string "*" >> return AnyElement
    , string "element()" >> return AnyElement
    , string "text()" >> return Text
    , string "@*" >> return AnyAttr
    , char '@' >> takeWhile (not . flip elem "/[") >>= return . Attr
    , takeWhile (not . flip elem "/[") >>= return . Element
    ]

cond :: Parser Cond
cond = do
    _ <- char '['
    _ <- char '@'
    rawName <- takeWhile (/= ' ')
    skipSpace
    _ <- char '='
    skipSpace
    _ <- char '\''
    val <- takeWhile (/= '\'')
    _ <- char '\''
    _ <- char ']'
    let name = fromString $ T.unpack rawName
    return $ AttrEq name val

root :: Cursor -> Cursor
root c =
    case parent c of
        [] -> c
        c':_ -> root c'

lift' :: XP -> Q Exp
lift' (XP fr ps) = do
    base <- if fr then fmap return [|root|] else return []
    ps' <- mapM liftPiece ps
    dot <- [|(>=>)|]
    let e = foldr1 (\a b -> InfixE (Just a) dot (Just b)) ps'
    e' <- if fr then [|$(return e) . root|] else return e
    case last ps of
        Piece _ Text _ -> return e'
        Piece _ AnyAttr _ -> return e'
        Piece _ Attr{} _ -> return e'
        Piece _ Element{} _ -> [|elem' <=< $(return e')|]
        Piece _ AnyElement _ -> [|elem' <=< $(return e')|]
        _ -> [|map node . $(return e')|]

elem' :: Cursor -> [X.Element]
elem' c =
    case node c of
        X.NodeElement e -> [e]
        _ -> []

checkAttr name val c =
    case node c of
        X.NodeElement (X.Element _ as _)
            | Just val == lookup name as -> [c]
        _ -> []

liftPiece :: Piece -> Q Exp
liftPiece (Piece axis nt conds) = do
    x <- if isAttr nt
            then liftNodeType nt
            else [|$(liftAxis axis) >=> $(liftNodeType nt)|]
    addConds x conds
  where
    isAttr Attr{} = True
    isAttr AnyAttr = True
    isAttr _ = False
    addConds x [] = return x
    addConds x (AttrEq y z:cs) = [|$(return x) >=> checkAttr $(liftName y) $(liftT z)|]

liftAxis :: Axis -> Q Exp
liftAxis Child = [|child|]
liftAxis Descendant = [|descendant|]

anyAttribute :: Cursor -> [(X.Name, Text)]
anyAttribute c =
    case node c of
        X.NodeElement (X.Element _ as _) -> as
        _ -> []

liftNodeType :: NodeType -> Q Exp
liftNodeType AnyNode = [|return|]
liftNodeType (Element t) = [|element $(liftName' t)|]
liftNodeType AnyElement = [|anyElement|]
liftNodeType Text = [|content|]
liftNodeType AnyAttr = [|anyAttribute|]
liftNodeType (Attr n) = [|attribute $(liftName' n)|]

liftName' :: Text -> Q Exp
liftName' = liftName . fromString . unpack
