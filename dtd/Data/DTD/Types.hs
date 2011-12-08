{-# LANGUAGE DeriveDataTypeable #-}
module Data.DTD.Types
  ( -- * DTD structure
    DTD (..)
  , DTDTextDecl (..)
  , DTDComponent (..)

    -- * Entity declarations and references
  , EntityDecl (..)

    -- * Element declarations
  , ElementDecl (..)
  , ContentDecl (..)
  , ContentModel (..)
  , Repeat (..)

    -- * Attribute declarations
  , AttList (..)
  , AttDecl (..)
  , AttType (..)
  , AttDefault (..)

    -- * Notation declarations
  , Notation (..)
  , NotationSource (..)
  )
  where

import Data.DTD.Types.Unresolved
    ( DTDTextDecl (..)
    , ContentModel (..)
    , Repeat (..)
    , AttType (..)
    , AttDefault (..)
    , Notation (..)
    , NotationSource (..)
    )
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.XML.Types (ExternalID, Instruction)

-- | A list of attribute declarations for an element.
data AttList =
     AttList
       { attListElementName :: Text -- ^ The name of the element to
                                    -- which the attribute
                                    -- declarations apply
       , attListDecls :: [AttDecl]
       }
  deriving (Show, Eq, Typeable)

-- | The content that can occur in an element.
data ContentDecl =
     ContentEmpty                   -- ^ No content
   | ContentAny                     -- ^ Unrestricted content
   | ContentElement ContentModel    -- ^ Structured element content
   | ContentMixed [Text]            -- ^ A mixture of text and elements
  deriving (Show, Eq, Typeable)

-- | A declaration of an element.
data ElementDecl =
     ElementDecl
      { eltDeclName :: Text
      , eltDeclContent :: ContentDecl
      }
  deriving (Show, Eq, Typeable)

data EntityDecl =
     InternalGeneralEntityDecl
       { entityDeclName :: Text
       , entityDeclValue :: Text
       }
   | ExternalGeneralEntityDecl
       { entityDeclName :: Text
       , entityDeclID :: ExternalID
       , entityDeclNotation :: Maybe Text
       }                                  -- ^ An external general
                                          -- entity is unparsed if a
                                          -- notation is specified.
  deriving (Show, Eq, Typeable)

-- | The kinds of components that can appear in a 'DTD'.
data DTDComponent =
     DTDEntityDecl EntityDecl   -- ^ Entity declaration
   | DTDElementDecl ElementDecl -- ^ Element declaration
   | DTDAttList AttList        -- ^ List of attribute declarions for
                                -- an element
   | DTDNotation Notation       -- ^ A notation declaration
   | DTDInstruction Instruction -- ^ A processing instruction
   | DTDComment Text            -- ^ A comment
  deriving (Show, Eq, Typeable)

-- | A 'DTD' is a sequence components in any order.
data DTD = DTD
             { dtdTextDecl :: Maybe DTDTextDecl
             , dtdComponents :: [DTDComponent]
             }
  deriving (Show, Eq, Typeable)

-- | A declaration of an attribute that can occur in an element.
data AttDecl =
     AttDecl
       { attDeclName :: Text           -- ^ The name of the attribute
       , attDeclType :: AttType   -- ^ The type of the attribute
       , attDeclDefault :: AttDefault  -- ^ The default value specification
       }
  deriving (Show, Eq)
