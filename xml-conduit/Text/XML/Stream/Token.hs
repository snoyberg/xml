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
import Data.String (IsString (fromString))
import Blaze.ByteString.Builder
    (Builder, fromByteString, writeByteString, copyByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)
import Blaze.ByteString.Builder.Char.Utf8 (writeChar, fromText)
import Data.Monoid (mconcat, mempty, mappend)
import Data.ByteString.Char8 ()
import Data.Map (Map)
import qualified Blaze.ByteString.Builder.Char8 as BC8
import qualified Data.Set as Set
import Data.List (foldl')
import Control.Arrow (first)

oneSpace :: Builder
oneSpace = copyByteString " "

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
    fromByteString "<?xml"
    `mappend` foldAttrs oneSpace attrs (fromByteString "?>")
tokenToBuilder (TokenInstruction (Instruction target data_)) = mconcat
    [ fromByteString "<?"
    , fromText target
    , fromByteString " "
    , fromText data_
    , fromByteString "?>"
    ]
tokenToBuilder (TokenBeginElement name attrs' isEmpty indent) =
      copyByteString "<"
    `mappend` tnameToText name
    `mappend` foldAttrs
        (if indent == 0 || lessThan3 attrs
            then oneSpace
            else BC8.fromString ('\n' : replicate indent ' '))
        attrs
        (if isEmpty then fromByteString "/>" else fromByteString ">")
  where
    attrs = nubAttrs $ map (first splitTName) attrs'
    lessThan3 [] = True
    lessThan3 [_] = True
    lessThan3 [_, _] = True
    lessThan3 _ = False
tokenToBuilder (TokenEndElement name) = mconcat
    [ fromByteString "</"
    , tnameToText name
    , fromByteString ">"
    ]
tokenToBuilder (TokenContent c) = contentToText c
tokenToBuilder (TokenCDATA t) = 
    copyByteString "<![CDATA["
    `mappend` escCDATA t
    `mappend` copyByteString "]]>"
tokenToBuilder (TokenComment t) = mconcat [fromByteString "<!--", fromText t, fromByteString "-->"]
tokenToBuilder (TokenDoctype name eid _) = mconcat
    [ fromByteString "<!DOCTYPE "
    , fromText name
    , go eid
    , fromByteString ">"
    ]
  where
    go Nothing = mempty
    go (Just (SystemID uri)) = mconcat
        [ fromByteString " SYSTEM \""
        , fromText uri
        , fromByteString "\""
        ]
    go (Just (PublicID pid uri)) = mconcat
        [ fromByteString " PUBLIC \""
        , fromText pid
        , fromByteString "\" \""
        , fromText uri
        , fromByteString "\""
        ]

data TName = TName (Maybe Text) Text
    deriving (Show, Eq, Ord)

tnameToText :: TName -> Builder
tnameToText (TName Nothing name) = fromText name
tnameToText (TName (Just prefix) name) = mconcat [fromText prefix, fromByteString ":", fromText name]

contentToText :: Content -> Builder
contentToText (ContentText t) =
    fromWriteList go $ T.unpack t
  where
    go '<' = writeByteString "&lt;"
    go '>' = writeByteString "&gt;"
    go '&' = writeByteString "&amp;"
    -- Not escaping quotes, since this is only called outside of attributes
    go c   = writeChar c
contentToText (ContentEntity e) = mconcat
    [ fromByteString "&"
    , fromText e
    , fromByteString ";"
    ]

type TAttribute = (TName, [Content])

foldAttrs :: Builder -- ^ before
          -> [TAttribute]
          -> Builder
          -> Builder
foldAttrs before attrs rest' =
    foldr go rest' attrs
  where
    go (key, val) rest =
      before
      `mappend` tnameToText key
      `mappend` copyByteString "=\""
      `mappend` foldr go' (fromByteString "\"" `mappend` rest) val
    go' (ContentText t) rest =
        fromWriteList h (T.unpack t) `mappend` rest
      where
        h '<' = writeByteString "&lt;"
        h '>' = writeByteString "&gt;"
        h '&' = writeByteString "&amp;"
        h '"' = writeByteString "&quot;"
        -- Not escaping single quotes, since our attributes are always double
        -- quoted
        h c   = writeChar c
    go' (ContentEntity t) rest =
        fromByteString "&"
        `mappend` fromText t
        `mappend` fromByteString ";"
        `mappend` rest

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
escCDATA s = fromText (T.replace "]]>" "]]]]><![CDATA[>" s)
