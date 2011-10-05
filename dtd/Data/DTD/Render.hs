{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      :  Data.XML.DTD.Render
-- Copyright   :  Suite Solutions Ltd., Israel 2011
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- A "Blaze.ByteString.Builder" renderer for XML Document Type
-- Declaration (DTD) documents.

{-
Copyright (c) 2011 Suite Solutions Ltd., Israel. All rights reserved.

For licensing information, see the BSD3-style license in the file
license.txt that was originally distributed by the author together
with this file.
-}

module Data.DTD.Render
  ( -- * DTD structure
    buildDTD
  , buildDTDTextDecl
  , buildDTDComponent

    -- * Entity declarations and references
  , buildEntityDecl

    -- * Element declarations
  , buildElementDecl
  , buildContentDecl
  , buildContentModel
  , buildRepeat

    -- * Attribute declarations
  , buildAttList
  , buildAttDecl
  , buildAttType
  , buildAttDefault

    -- * Notation declarations
  , buildNotation
  , buildNotationSource

    -- * Comments and processing instructions
  , buildInstruction
  , buildComment

    -- * Builder combinators for general DTD syntax
  , buildExternalID
  , buildList
  , buildChoice
  , buildMaybe
  , newline
  , space
  , quote
  , pbracket
  , parens
  ) 
  where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromChar)
import Data.DTD.Types
import Data.XML.Types (ExternalID(..), Instruction(..))
import Data.Text (Text)
import Data.Monoid (Monoid(..))
import Data.List (intersperse)
import System.IO (nativeNewline, Newline(CRLF))
-- No instance Semigroup Builder yet, so <> defined here manually.
--import Data.Semigroup ((<>))
(<>) = mappend

-- | Build an optional item.
buildMaybe :: (a -> Builder) -> Maybe a -> Builder
buildMaybe = maybe mempty

-- | Build a newline.
newline :: Builder
newline = fromText $ case nativeNewline of
                       CRLF   -> "\r\n"
                       _      -> "\n"

-- | Build a space.
space :: Builder
space = fromChar ' '

-- | Build a quoted string.
quote :: Builder -> Builder
quote = (fromChar '"' <>) . (<> fromChar '"')

-- | Build a string quoted by angle brackets, with an exclamation mark.
pbracket :: Builder -> Builder
pbracket = (fromText "<!" <>) . (<> fromChar '>')

-- | Build a string surround by parantheses.
parens :: Builder -> Builder
parens = (fromChar '(' <>) . (<> fromChar ')')

-- | Build a list of items
buildList :: Text -> (a -> Builder) -> [a] -> Builder
buildList sep build =
  parens . mconcat . intersperse (fromText sep) . map build

-- | Build a choice expression.
buildChoice :: (a -> Builder) -> [a] -> Builder
buildChoice = buildList " | "

-- | A 'Builder' for a 'DTD'.
buildDTD (DTD decl cmps) = buildMaybe buildDTDTextDecl decl <>
  mconcat (map ((<> newline) . buildDTDComponent) cmps)

-- | A 'Builder' for a 'DTDTextDecl'.
buildDTDTextDecl :: DTDTextDecl -> Builder
buildDTDTextDecl (DTDTextDecl ver enc) = fromText "<?xml " <>
  buildMaybe
    ((fromText "version=" <>) . (<> space) . quote . fromText) ver <>
  fromText "encoding=" <> quote (fromText enc) <> fromText "?>" <> newline

-- | A 'Builder' for a 'DTDComponent'.
buildDTDComponent :: DTDComponent -> Builder
buildDTDComponent (DTDEntityDecl d)  = buildEntityDecl d
buildDTDComponent (DTDElementDecl d) = buildElementDecl d
buildDTDComponent (DTDAttList a)     = buildAttList a
buildDTDComponent (DTDNotation n)    = buildNotation n
buildDTDComponent (DTDInstruction i) = buildInstruction i
buildDTDComponent (DTDComment c)     = buildComment c

-- | A 'Builder' for an 'EntityDecl'.
buildEntityDecl :: EntityDecl -> Builder
buildEntityDecl d = pbracket $ fromText "ENTITY " <> pct <> name <> val
  where
    name = fromText (entityDeclName d) <> space
    (pct, val) = case d of
      InternalGeneralEntityDecl   _ val    -> (mempty, quote $ fromText val)
      ExternalGeneralEntityDecl   _ eid nt -> (mempty, ege eid nt)
    pctBld = fromText "% "
    ege eid nt = buildExternalID eid <>
      buildMaybe ((fromText " NDATA " <>) . quote . fromText) nt

-- | A 'Builder' for an 'ExternalID'.
buildExternalID :: ExternalID -> Builder
buildExternalID (SystemID sys)     = fromText "SYSTEM " <>
                                     quote (fromText sys)
buildExternalID (PublicID pub sys) = fromText "PUBLIC " <>
                                     quote (fromText pub) <> space <>
                                     quote (fromText sys)

-- | A 'Builder' for an 'ElementDecl'.
buildElementDecl :: ElementDecl -> Builder
buildElementDecl (ElementDecl name content) = pbracket $
  fromText "ELEMENT " <> fromText name <> space <> buildContentDecl content

-- | A 'Builder' for a 'ContentDecl'.
buildContentDecl :: ContentDecl -> Builder
buildContentDecl ContentEmpty         = fromText "EMPTY"
buildContentDecl ContentAny           = fromText "ANY"
buildContentDecl (ContentElement cm)  = buildContentModel cm
buildContentDecl (ContentMixed names) =
  buildChoice fromText ("#PCDATA" : names) <> fromChar '*'

-- | A 'Builder' for a 'ContentModel'.
buildContentModel :: ContentModel -> Builder
buildContentModel (CMName nam rpt) = parens $ fromText nam <> buildRepeat rpt
buildContentModel cm               = buildCM cm
  where
    buildCM (CMName name  rpt) = fromText name <> buildRepeat rpt
    buildCM (CMChoice cms rpt) = cp buildChoice      cms rpt
    buildCM (CMSeq    cms rpt) = cp (buildList ", ") cms rpt
    cp f cms rpt = f buildCM cms <> buildRepeat rpt

-- | A 'Builder' for a 'Repeat'.
buildRepeat :: Repeat -> Builder
buildRepeat One        = mempty
buildRepeat ZeroOrOne  = fromChar '?'
buildRepeat ZeroOrMore = fromChar '*'
buildRepeat OneOrMore  = fromChar '+'

-- | A 'Builder' for an 'AttList'.
buildAttList :: AttList -> Builder
buildAttList (AttList name decls) = pbracket $
  fromText "ATTLIST " <> fromText name <> mconcat
  (map ((newline <>) . (fromText "          " <>) . buildAttDecl) decls)

-- | A 'Builder' for an 'AttDecl'.
buildAttDecl :: AttDecl -> Builder
buildAttDecl (AttDecl name typ dflt) = fromText name <> space <>
  buildAttType typ <> space <> buildAttDefault dflt

-- | A 'Builder' for an 'AttType'.
buildAttType :: AttType -> Builder
buildAttType AttStringType        = fromText "CDATA"
buildAttType AttIDType            = fromText "ID"
buildAttType AttIDRefType         = fromText "IDREF"
buildAttType AttIDRefsType        = fromText "IDREFS"
buildAttType AttEntityType        = fromText "ENTITY"
buildAttType AttEntitiesType      = fromText "ENTITIES"
buildAttType AttNmTokenType       = fromText "NMTOKEN"
buildAttType AttNmTokensType      = fromText "NMTOKENS"
buildAttType (AttEnumType vs)     = buildChoice fromText vs
buildAttType (AttNotationType ns) = fromText "NOTATION " <>
                                    buildChoice fromText ns

-- | A 'Builder' for an 'AttDefault'.
buildAttDefault :: AttDefault -> Builder
buildAttDefault AttRequired           = fromText "#REQUIRED"
buildAttDefault AttImplied            = fromText "#IMPLIED"
buildAttDefault (AttDefaultValue val) = quote (fromText val)
buildAttDefault (AttFixed val)        = fromText "#FIXED " <>
                                        quote (fromText val)

-- | A 'Builder' for a 'Notation'.
buildNotation :: Notation -> Builder
buildNotation (Notation name src) = pbracket $
  fromText "NOTATION " <> fromText name <> space <> buildNotationSource src

-- | A 'Builder' for a 'NotationSource'.
buildNotationSource :: NotationSource -> Builder
buildNotationSource (NotationSysID sys)        = fromText "SYSTEM " <>
                                                 quote (fromText sys)
buildNotationSource (NotationPubID pub)        = fromText "PUBLIC " <>
                                                 quote (fromText pub)
buildNotationSource (NotationPubSysID pub sys) = fromText "PUBLIC " <>
                                                 quote (fromText pub) <>
                                                 space <>
                                                 quote (fromText sys)

-- | A 'Builder' for an 'Instruction'.
buildInstruction :: Instruction -> Builder
buildInstruction (Instruction trgt dat) =
  fromText "<?" <> fromText trgt <> space <> fromText dat <> fromText "?>"

-- | A 'Builder' for a comment. The comment text cannot be null,
-- cannot contain two consecutive '-', and cannot end in '-'.
buildComment :: Text -> Builder
buildComment cmt = pbracket $ fromText "--" <> fromText cmt <> fromText "--"
