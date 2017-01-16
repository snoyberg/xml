{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Hamlet.XML
    ( xml
    , xmlFile
    , ToAttributes (..)
    ) where

#if MIN_VERSION_template_haskell(2,9,0)
import Language.Haskell.TH.Syntax hiding (Module)
#else
import Language.Haskell.TH.Syntax
#endif
import Language.Haskell.TH.Quote
import Data.Char (isDigit)
import qualified Data.Text.Lazy as TL
import Control.Monad ((<=<))
import Text.Hamlet.XMLParse
import Text.Shakespeare.Base (readUtf8File, derefToExp, Scope, Deref, Ident (Ident))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Text.XML as X
import Data.String (fromString)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Arrow (first, (***))
import Data.List (intercalate)

-- | Convert some value to a list of attribute pairs.
class ToAttributes a where
    toAttributes :: a -> Map.Map X.Name Text
instance ToAttributes (X.Name, Text) where
    toAttributes (k, v) = Map.singleton k v
instance ToAttributes (Text, Text) where
    toAttributes (k, v) = Map.singleton (fromString $ unpack k) v
instance ToAttributes (String, String) where
    toAttributes (k, v) = Map.singleton (fromString k) (pack v)
instance ToAttributes [(X.Name, Text)] where
    toAttributes = Map.fromList
instance ToAttributes [(Text, Text)] where
    toAttributes = Map.fromList . map (first (fromString . unpack))
instance ToAttributes [(String, String)] where
    toAttributes = Map.fromList . map (fromString *** pack)
instance ToAttributes (Map.Map X.Name Text) where
    toAttributes = id
instance ToAttributes (Map.Map Text Text) where
    toAttributes = Map.mapKeys (fromString . unpack)
instance ToAttributes (Map.Map String String) where
    toAttributes = Map.mapKeys fromString . Map.map pack

docsToExp :: Scope -> [Doc] -> Q Exp
docsToExp scope docs = [| concat $(fmap ListE $ mapM (docToExp scope) docs) |]

unIdent :: Ident -> String
unIdent (Ident s) = s

bindingPattern :: Binding -> Q (Pat, [(Ident, Exp)])
bindingPattern (BindAs i@(Ident s) b) = do
    name <- newName s
    (pattern, scope) <- bindingPattern b
    return (AsP name pattern, (i, VarE name):scope)
bindingPattern (BindVar i@(Ident s))
    | s == "_" = return (WildP, [])
    | all isDigit s = do
        return (LitP $ IntegerL $ read s, [])
    | otherwise = do
        name <- newName s
        return (VarP name, [(i, VarE name)])
bindingPattern (BindTuple is) = do
    (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
    return (TupP patterns, concat scopes)
bindingPattern (BindList is) = do
    (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
    return (ListP patterns, concat scopes)
bindingPattern (BindConstr con is) = do
    (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
    return (ConP (mkConName con) patterns, concat scopes)
bindingPattern (BindRecord con fields wild) = do
    let f (Ident field,b) =
           do (p,s) <- bindingPattern b
              return ((mkName field,p),s)
    (patterns, scopes) <- fmap unzip $ mapM f fields
    (patterns1, scopes1) <- if wild
       then bindWildFields con $ map fst fields
       else return ([],[])
    return (RecP (mkConName con) (patterns++patterns1), concat scopes ++ scopes1)

mkConName :: DataConstr -> Name
mkConName = mkName . conToStr

conToStr :: DataConstr -> String
conToStr (DCUnqualified (Ident x)) = x
conToStr (DCQualified (Module xs) (Ident x)) = intercalate "." $ xs ++ [x]

-- Wildcards bind all of the unbound fields to variables whose name
-- matches the field name.
--
-- For example: data R = C { f1, f2 :: Int }
-- C {..}           is equivalent to   C {f1=f1, f2=f2}
-- C {f1 = a, ..}   is equivalent to   C {f1=a,  f2=f2}
-- C {f2 = a, ..}   is equivalent to   C {f1=f1, f2=a}
bindWildFields :: DataConstr -> [Ident] -> Q ([(Name, Pat)], [(Ident, Exp)])
bindWildFields conName fields = do
  fieldNames <- recordToFieldNames conName
  let available n     = nameBase n `notElem` map unIdent fields
  let remainingFields = filter available fieldNames
  let mkPat n = do
        e <- newName (nameBase n)
        return ((n,VarP e), (Ident (nameBase n), VarE e))
  fmap unzip $ mapM mkPat remainingFields

-- Important note! reify will fail if the record type is defined in the
-- same module as the reify is used. This means quasi-quoted Hamlet
-- literals will not be able to use wildcards to match record types
-- defined in the same module.
recordToFieldNames :: DataConstr -> Q [Name]
recordToFieldNames conStr = do
  -- use 'lookupValueName' instead of just using 'mkName' so we reify the
  -- data constructor and not the type constructor if their names match.
  Just conName                <- lookupValueName $ conToStr conStr
#if MIN_VERSION_template_haskell(2,11,0)
  DataConI _ _ typeName         <- reify conName
  TyConI (DataD _ _ _ _ cons _) <- reify typeName
#else
  DataConI _ _ typeName _     <- reify conName
  TyConI (DataD _ _ _ cons _) <- reify typeName
#endif
  [fields] <- return [fields | RecC name fields <- cons, name == conName]
  return [fieldName | (fieldName, _, _) <- fields]

docToExp :: Scope -> Doc -> Q Exp
docToExp scope (DocTag name attrs attrsD cs) =
    [| [ X.NodeElement (X.Element ($(liftName name)) $(mkAttrs scope attrs attrsD) $(docsToExp scope cs))
       ] |]
docToExp _ (DocContent (ContentRaw s)) = [| [ X.NodeContent (pack $(lift s)) ] |]
docToExp scope (DocContent (ContentVar d)) = [| [ X.NodeContent $(return $ derefToExp scope d) ] |]
docToExp scope (DocContent (ContentEmbed d)) = return $ derefToExp scope d
docToExp scope (DocForall list idents inside) = do
    let list' = derefToExp scope list
    (pat, extraScope) <- bindingPattern idents
    let scope' = extraScope ++ scope
    mh <- [|F.concatMap|]
    inside' <- docsToExp scope' inside
    let lam = LamE [pat] inside'
    return $ mh `AppE` lam `AppE` list'
docToExp scope (DocWith [] inside) = docsToExp scope inside
docToExp scope (DocWith ((deref, idents):dis) inside) = do
    let deref' = derefToExp scope deref
    (pat, extraScope) <- bindingPattern idents
    let scope' = extraScope ++ scope
    inside' <- docToExp scope' (DocWith dis inside)
    let lam = LamE [pat] inside'
    return $ lam `AppE` deref'
docToExp scope (DocMaybe val idents inside mno) = do
    let val' = derefToExp scope val
    (pat, extraScope) <- bindingPattern idents
    let scope' = extraScope ++ scope
    inside' <- docsToExp scope' inside
    let inside'' = LamE [pat] inside'
    ninside' <- case mno of
                    Nothing -> [| [] |]
                    Just no -> docsToExp scope no
    [| maybe $(return ninside') $(return inside'') $(return val') |]
docToExp scope (DocCond conds final) = do
    unit <- [| () |]
    otherwise' <- [|otherwise|]
    body <- fmap GuardedB $ mapM go $ map (first (derefToExp scope)) conds ++ [(otherwise', fromMaybe [] final)]
    return $ CaseE unit [Match (TupP []) body []]
  where
    go (deref, inside) = do
        inside' <- docsToExp scope inside
        return (NormalG deref, inside')
docToExp scope (DocCase deref cases) = do
    let exp_ = derefToExp scope deref
    matches <- mapM toMatch cases
    return $ CaseE exp_ matches
  where
    toMatch :: (Binding, [Doc]) -> Q Match
    toMatch (idents, inside) = do
        (pat, extraScope) <- bindingPattern idents
        let scope' = extraScope ++ scope
        insideExp <- docsToExp scope' inside
        return $ Match pat (NormalB insideExp) []

mkAttrs :: Scope -> [(Maybe Deref, String, [Content])] -> [Deref] -> Q Exp
mkAttrs _ [] [] = [| Map.empty |]
mkAttrs scope [] (deref:rest) = do
    rest' <- mkAttrs scope [] rest
    [| Map.union (toAttributes $(return $ derefToExp scope deref)) $(return rest') |]
mkAttrs scope ((mderef, name, value):rest) attrs = do
    rest' <- mkAttrs scope rest attrs
    this <- [| Map.insert $(liftName name) (T.concat $(fmap ListE $ mapM go value)) |]
    let with = [| $(return this) $(return rest') |]
    case mderef of
        Nothing -> with
        Just deref -> [| if $(return $ derefToExp scope deref) then $(with) else $(return rest') |]
  where
    go (ContentRaw s) = [| pack $(lift s) |]
    go (ContentVar d) = return $ derefToExp scope d
    go ContentEmbed{} = error "Cannot use embed interpolation in attribute value"

liftName :: String -> Q Exp
liftName s = do
    X.Name local mns _ <- return $ fromString s
    case mns of
        Nothing -> [| X.Name (pack $(lift $ unpack local)) Nothing Nothing |]
        Just ns -> [| X.Name (pack $(lift $ unpack local)) (Just $ pack $(lift $ unpack ns)) Nothing |]

xml :: QuasiQuoter
xml = QuasiQuoter { quoteExp = strToExp }

xmlFile :: FilePath -> Q Exp
xmlFile = strToExp . TL.unpack <=< qRunIO . readUtf8File

strToExp :: String -> Q Exp
strToExp s =
    case parseDoc s of
        Error e -> error e
        Ok x -> docsToExp [] x

