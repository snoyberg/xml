------------------------------------------------------------------------------
-- |
-- Module      :  Data.XML.DTD.Types
-- Copyright   :  Suite Solutions Ltd., Israel 2011
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- This module provides types to represent an XML Document Type
-- Declaration (DTD) as defined in W3C specifications
-- (<http://www.w3.org/XML/Core/#Publications>). It is intended to be
-- compatible with and extend the set of types in "Data.XML.Types"
-- provided by the xml-types package.
--
-- Following the philosophy of @Data.XML.Types@, the types in this
-- module are not intended to be a strict and complete representation
-- of the model in the W3C specifications; rather, they are intended
-- to be convenient and type-safe for the kinds of processing of DTDs
-- that are commonly done in practice. As such, this model is
-- compatible with both Version 1.0 and Version 1.1 of the XML
-- specification.
--
-- Therefore, these types are not suitable for type-level validation
-- of the syntax of a DTD. For example: these types are more
-- lenient than the specs about the characters that are allowed in
-- various locations in a DTD; entities of various kinds only appear
-- as distinct syntactic elements in places where they are commonly
-- needed when processing DTDs; etc.
--
-- Conditional sections are not represented in these types. They
-- should be handled directly by parsers and renderers, if needed.

{-
Copyright (c) 2011 Suite Solutions Ltd., Israel. All rights reserved.

For licensing information, see the BSD3-style license in the file
license.txt that was originally distributed by the author together
with this file.
-}

module Data.DTD.Types.Unresolved
  ( -- * DTD structure
    DTD (..)
  , DTDTextDecl (..)
  , DTDComponent (..)

    -- * Entity declarations and references
  , EntityDecl (..)
  , EntityValue (..)
  , PERef

    -- * Element declarations
  , ElementDecl (..)
  , ContentDecl (..)
  , ContentModel (..)
  , Repeat (..)

    -- * Attribute declarations
  , AttList (..)
  , AttDecl (..)
  , AttDeclPERef (..)
  , AttType (..)
  , AttTypePERef (..)
  , AttDefault (..)

    -- * Notation declarations
  , Notation (..)
  , NotationSource (..)
  ) 
  where

import Data.Text (Text)
import Data.Typeable ( Typeable, TypeRep, typeOf
                     , mkTyConApp, mkTyCon)
import Data.XML.Types (ExternalID, Instruction)

-- | A 'DTD' is a sequence components in any order.
data DTD = DTD
             { dtdTextDecl :: Maybe DTDTextDecl
             , dtdComponents :: [DTDComponent]
             }
  deriving (Show, Eq)

instance Typeable DTD where
  typeOf = typeString "DTD"

-- | The @?xml@ text declaration at the beginning of a DTD.
data DTDTextDecl =
     DTDTextDecl
       { dtdXMLVersion :: Maybe Text
       , dtdEncoding :: Text
       }
  deriving (Show, Eq)

instance Typeable DTDTextDecl where
  typeOf = typeString "DTDTextDecl"

-- | The kinds of components that can appear in a 'DTD'.
data DTDComponent =
     DTDEntityDecl EntityDecl   -- ^ Entity declaration
   | DTDElementDecl ElementDecl -- ^ Element declaration
   | DTDAttList AttList        -- ^ List of attribute declarions for
                                -- an element
   | DTDNotation Notation       -- ^ A notation declaration
   | DTDPERef PERef             -- ^ A parameter entity reference in
                                -- the top-level flow of the DTD
   | DTDInstruction Instruction -- ^ A processing instruction
   | DTDComment Text            -- ^ A comment
  deriving (Show, Eq)

instance Typeable DTDComponent where
  typeOf = typeString "DTDComponent"

-- | A declaration of an entity. An entity is a textual substitution
-- variable. General entities can be referenced in an XML document
-- conforming to the DTD, and parameter entities can be referenced in
-- the DTD itself. The value of an unparsed entity is not specified in
-- the DTD; it is specified by external syntax declared as a notation
-- elsewhere in the DTD.
data EntityDecl =
     InternalGeneralEntityDecl
       { entityDeclName :: Text
       , entityDeclValue :: [EntityValue]
       }
   | ExternalGeneralEntityDecl
       { entityDeclName :: Text
       , entityDeclID :: ExternalID
       , entityDeclNotation :: Maybe Text
       }                                  -- ^ An external general
                                          -- entity is unparsed if a
                                          -- notation is specified.
   | InternalParameterEntityDecl
       { entityDeclName :: Text
       , entityDeclValue :: [EntityValue]
       }
   | ExternalParameterEntityDecl
       { entityDeclName :: Text
       , entityDeclID :: ExternalID
       }
  deriving (Show, Eq)

instance Typeable EntityDecl where
  typeOf = typeString "EntityDecl"

-- | The value of an internal entity may contain references to
-- parameter entities; these references need to be resolved to obtain
-- the actual replacement value of the entity. So we represent the
-- value as a mixture of parameter entity references and free text.
data EntityValue =
     EntityText Text
   | EntityPERef PERef
  deriving (Show, Eq)

instance Typeable EntityValue where
  typeOf = typeString "EntityValue"

-- | A parameter entity reference. It contains the name of the
-- parameter entity that is being referenced.
type PERef = Text

-- | A declaration of an element.
data ElementDecl =
     ElementDecl
      { eltDeclName :: Either PERef Text
      , eltDeclContent :: ContentDecl
      }
  deriving (Show, Eq)

instance Typeable ElementDecl where
  typeOf = typeString "ElementDecl"

-- | The content that can occur in an element.
data ContentDecl =
     ContentEmpty                   -- ^ No content
   | ContentAny                     -- ^ Unrestricted content
   | ContentElement [EntityValue]   -- ^ Structured element content
   | ContentMixed [Text]            -- ^ A mixture of text and elements
   | ContentPERef PERef
  deriving (Show, Eq)

instance Typeable ContentDecl where
  typeOf = typeString "ContentDecl"

-- | A model of structured content for an element.
data ContentModel =
     CMName Text Repeat             -- ^ Element name
   | CMChoice [ContentModel] Repeat -- ^ Choice, delimited by @\"|\"@
   | CMSeq [ContentModel] Repeat    -- ^ Sequence, delimited by @\",\"@
  deriving (Show, Eq)

instance Typeable ContentModel where
  typeOf = typeString "ContentModel"

-- | The number of times a production of content model syntax can
-- repeat.
data Repeat = One | ZeroOrOne | ZeroOrMore | OneOrMore
  deriving (Show, Eq)

instance Typeable Repeat where
  typeOf = typeString "Repeat"

-- | A list of attribute declarations for an element.
data AttList =
     AttList
       { attListElementName :: Either PERef Text -- ^ The name of the element to
                                    -- which the attribute
                                    -- declarations apply
       , attListDecls :: [AttDeclPERef]
       }
  deriving (Show, Eq)

instance Typeable AttList where
  typeOf = typeString "AttList"

data AttDeclPERef = ADPDecl AttDecl | ADPPERef PERef
    deriving (Show, Eq)

-- | A declaration of an attribute that can occur in an element.
data AttDecl =
     AttDecl
       { attDeclName :: Text           -- ^ The name of the attribute
       , attDeclType :: AttTypePERef   -- ^ The type of the attribute
       , attDeclDefault :: AttDefault  -- ^ The default value specification
       }
  deriving (Show, Eq)

instance Typeable AttDecl where
  typeOf = typeString "AttDecl"

data AttTypePERef = ATPType AttType | ATPPERef PERef
    deriving (Show, Eq)

-- | The type of value that an attribute can take.
data AttType =
     AttStringType           -- ^ Any text
   | AttIDType               -- ^ A unique ID
   | AttIDRefType            -- ^ A reference to an ID
   | AttIDRefsType           -- ^ One or more references to IDs
   | AttEntityType           -- ^ An unparsed external entity
   | AttEntitiesType         -- ^ One or more unparsed external entities
   | AttNmTokenType          -- ^ A name-like token
   | AttNmTokensType         -- ^ One or more name-like tokens
   | AttEnumType [Text]      -- ^ One of the given values
   | AttNotationType [Text]  -- ^ Specified by external syntax
                             -- declared as a notation
  deriving (Show, Eq)

instance Typeable AttType where
  typeOf = typeString "AttType"

-- | A default value specification for an attribute.
data AttDefault =
     AttRequired          -- ^ No default value; the attribute must always
                          -- be supplied
   | AttImplied           -- ^ No default value; the attribute is optional
   | AttFixed Text        -- ^ When supplied, the attribute must have the
                          -- given value
   | AttDefaultValue Text -- ^ The attribute has the given default value
                          -- when not supplied
  deriving (Show, Eq)

instance Typeable AttDefault where
  typeOf = typeString "AttDefault"

-- | A declaration of a notation.
data Notation =
     Notation
       { notationName :: Text,
         notationSource :: NotationSource
       }
  deriving (Show, Eq)

instance Typeable Notation where
  typeOf = typeString "Notation"

-- | A source for a notation. We do not use the usual 'ExternalID'
-- type here, because for notations it is only optional, not required,
-- for a public ID to be accompanied also by a system ID.
data NotationSource =
     NotationSysID Text         -- ^ A system ID
   | NotationPubID Text         -- ^ A public ID
   | NotationPubSysID Text Text -- ^ A public ID with a system ID
  deriving (Show, Eq)

instance Typeable NotationSource where
  typeOf = typeString "NotationSource"

typeString :: String -> a -> TypeRep
typeString str _ = mkTyConApp (mkTyCon ("Data.XML.DTD.Types." ++ str)) []
