{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Stream.Token
    ( tokenToBuilder
    , TName (..)
    , Token (..)
    , TAttribute
    , NSLevel (..)
    ) where

import Data.XML.Types (Instruction (..), Content (..), ExternalID (..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder, encodeUtf8BuilderEscaped)
import Data.String (IsString (fromString))
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Prim as E
import Data.ByteString.Builder.Prim ((>*<), (>$<), condB)
import Data.Monoid (mconcat, mempty, (<>))
import Data.Map (Map)
import qualified Data.Set as Set
import Data.List (foldl')
import Control.Arrow (first)
import Data.Word (Word8)

oneSpace :: Builder
oneSpace = " "

data Token = TokenXMLDeclaration [TAttribute]
           | TokenInstruction Instruction
           | TokenBeginElement TName [TAttribute] Bool Int -- ^ indent
           | TokenEndElement TName
           | TokenContent Content
           | TokenComment Text
           | TokenDoctype Text (Maybe ExternalID) [(Text, Text)]
           | TokenCDATA Text
    deriving Show
tokenToBuilder :: Token -> Builder
tokenToBuilder (TokenXMLDeclaration attrs) =
    "<?xml" <>
    foldAttrs oneSpace attrs <>
    "?>"
tokenToBuilder (TokenInstruction (Instruction target data_)) =
    "<?" <>
    encodeUtf8Builder target <>
    " " <>
    encodeUtf8Builder data_ <>
    "?>"
tokenToBuilder (TokenBeginElement name attrs' isEmpty indent) =
    "<" <>
    tnameToText name <>
    foldAttrs
        (if indent == 0 || lessThan3 attrs
            then oneSpace
            else mconcat $ ("\n" : replicate indent " "))
        attrs <>
    (if isEmpty then "/>" else ">")
  where
    attrs = nubAttrs $ map (first splitTName) attrs'
    lessThan3 [] = True
    lessThan3 [_] = True
    lessThan3 [_, _] = True
    lessThan3 _ = False
tokenToBuilder (TokenEndElement name) = "</" <> tnameToText name <> ">"
tokenToBuilder (TokenContent c) = contentToText c
tokenToBuilder (TokenCDATA t) = "<![CDATA[" <> escCDATA t <> "]]>"
tokenToBuilder (TokenComment t) = "<!--" <> encodeUtf8Builder t <> "-->"
tokenToBuilder (TokenDoctype name eid _) =
    "<!DOCTYPE " <>
    encodeUtf8Builder name <>
    go eid <>
    ">"
  where
    go Nothing = mempty
    go (Just (SystemID uri)) = " SYSTEM \"" <> encodeUtf8Builder uri <> "\""
    go (Just (PublicID pid uri)) =
        " PUBLIC \"" <>
        encodeUtf8Builder pid <>
        "\" \"" <>
        encodeUtf8Builder uri <>
        "\""

data TName = TName (Maybe Text) Text
    deriving (Show, Eq, Ord)

tnameToText :: TName -> Builder
tnameToText (TName Nothing name) = encodeUtf8Builder name
tnameToText (TName (Just prefix) name) =
  encodeUtf8Builder prefix <> ":" <> encodeUtf8Builder name

contentToText :: Content -> Builder
contentToText (ContentText t) = encodeUtf8BuilderEscaped (charUtf8XmlEscaped ECContent) t
contentToText (ContentEntity e) = "&" <> encodeUtf8Builder e <> ";"

-- | What usage are we escaping for?
data EscapeContext = ECContent   -- ^ <el>..</el>
                   | ECDoubleArg -- ^ <el arg=".." />
                   | ECSingleArg -- ^ <el arg='..' />
  deriving (Show, Eq)

{-# INLINE charUtf8XmlEscaped #-}
charUtf8XmlEscaped :: EscapeContext -> E.BoundedPrim Word8
charUtf8XmlEscaped ec =
                          (condB (>  _gt) (E.liftFixedToBounded E.word8)) $
                          (condB (== _lt) (fixed4 (_am,(_l,(_t,_sc))))) $           -- &lt;
    escapeFor ECContent   (condB (== _gt) (fixed4 (_am,(_g,(_t,_sc))))) $           -- &gt;
                          (condB (== _am) (fixed5 (_am,(_a,(_m,(_p,_sc)))))) $      -- &amp;
    escapeFor ECDoubleArg (condB (== _dq) (fixed6 (_am,(_q,(_u,(_o,(_t,_sc))))))) $ -- &quot;
    escapeFor ECSingleArg (condB (== _sq) (fixed6 (_am,(_a,(_p,(_o,(_s,_sc))))))) $ -- &apos;
    (E.liftFixedToBounded E.word8)         -- fallback for Chars smaller than '>'
  where
    _gt = 62 -- >
    _lt = 60 -- <
    _am = 38 -- &
    _dq = 34 -- "
    _sq = 39 -- '
    _l  = 108 -- l
    _t  = 116 -- t
    _g  = 103 -- g
    _a  = 97  -- a
    _m  = 109 -- m
    _p  = 112 -- p
    _o  = 111 -- o
    _s  = 115 -- s
    _q  = 113 -- q
    _u  = 117 -- u
    _sc = 59  -- ;

    {-# INLINE escapeFor #-}
    escapeFor :: EscapeContext -> (a -> a) -> a -> a
    escapeFor ec' f a
      | ec == ec' = f a
      | otherwise = a

    {-# INLINE fixed4 #-}
    fixed4 x = E.liftFixedToBounded $ const x >$<
      E.word8 >*< E.word8 >*< E.word8 >*< E.word8

    {-# INLINE fixed5 #-}
    fixed5 x = E.liftFixedToBounded $ const x >$<
      E.word8 >*< E.word8 >*< E.word8 >*< E.word8 >*< E.word8

    {-# INLINE fixed6 #-}
    fixed6 x = E.liftFixedToBounded $ const x >$<
      E.word8 >*< E.word8 >*< E.word8 >*< E.word8 >*< E.word8 >*< E.word8

type TAttribute = (TName, [Content])

foldAttrs :: Builder -- ^ before
          -> [TAttribute]
          -> Builder
foldAttrs before =
    foldMap go
  where
    go (key, val) =
      before <>
      tnameToText key <>
      "=\"" <>
      foldMap go' val <>
      "\""
    go' (ContentText t) =
      encodeUtf8BuilderEscaped (charUtf8XmlEscaped ECDoubleArg) t
    go' (ContentEntity t) = "&" <> encodeUtf8Builder t <> ";"

instance IsString TName where
    fromString = TName Nothing . T.pack

data NSLevel = NSLevel
    { defaultNS :: Maybe Text
    , prefixes :: Map Text Text
    }
    deriving Show

nubAttrs :: [TAttribute] -> [TAttribute]
nubAttrs orig =
    front []
  where
    (front, _) = foldl' go (id, Set.empty) orig
    go (dlist, used) (k, v)
        | k `Set.member` used = (dlist, used)
        | otherwise = (dlist . ((k, v):), Set.insert k used)

splitTName :: TName -> TName
splitTName x@(TName Just{} _) = x
splitTName x@(TName Nothing t)
    | T.null b = x
    | otherwise = TName (Just a) $ T.drop 1 b
  where
    (a, b) = T.break (== ':') t

escCDATA :: Text -> Builder
escCDATA s = encodeUtf8Builder (T.replace "]]>" "]]]]><![CDATA[>" s)
