{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Enumerator.Token
    ( tokenToBuilder
    , TName (..)
    , Token (..)
    , TAttribute
    ) where

import Data.XML.Types (Instruction (..), Content (..), ExternalID (..))
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.String
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Blaze.ByteString.Builder.Html.Utf8
import Data.Monoid
import Data.ByteString.Char8 ()

data Token = TokenBeginDocument [TAttribute]
           | TokenInstruction Instruction
           | TokenBeginElement TName [TAttribute] Bool
           | TokenEndElement TName
           | TokenContent Content
           | TokenComment Text
           | TokenDoctype Text (Maybe ExternalID)
    deriving Show
tokenToBuilder :: Token -> Builder
tokenToBuilder (TokenBeginDocument attrs) =
    mconcat $ fromByteString "<?xml" : foldAttrs attrs [fromByteString "?>\n"]
tokenToBuilder (TokenInstruction (Instruction target data_)) = mconcat
    [ fromByteString "<?"
    , fromLazyText target
    , fromByteString " "
    , fromLazyText data_
    , fromByteString "?>"
    ]
tokenToBuilder (TokenBeginElement name attrs isEmpty) = mconcat
    $ fromByteString "<"
    : tnameToText name
    : foldAttrs attrs
    [ if isEmpty then fromByteString "/>" else fromByteString ">"
    ]
tokenToBuilder (TokenEndElement name) = mconcat
    [ fromByteString "</"
    , tnameToText name
    , fromByteString ">"
    ]
tokenToBuilder (TokenContent c) = contentToText c
tokenToBuilder (TokenComment t) = mconcat [fromByteString "<!--", fromLazyText t, fromByteString "-->"]
tokenToBuilder (TokenDoctype name eid) = mconcat
    [ fromByteString "<!DOCTYPE "
    , fromLazyText name
    , go eid
    , fromByteString ">\n"
    ]
  where
    go Nothing = mempty
    go (Just (SystemID uri)) = mconcat
        [ fromByteString " SYSTEM \""
        , fromLazyText uri
        , fromByteString "\""
        ]
    go (Just (PublicID pid uri)) = mconcat
        [ fromByteString " PUBLIC \""
        , fromLazyText pid
        , fromByteString "\" \""
        , fromLazyText uri
        , fromByteString "\""
        ]

data TName = TName (Maybe Text) Text
    deriving Show

tnameToText :: TName -> Builder
tnameToText (TName Nothing name) = fromLazyText name
tnameToText (TName (Just prefix) name) = mconcat [fromLazyText prefix, fromByteString ":", fromLazyText name]

contentToText :: Content -> Builder
contentToText (ContentText t) = fromHtmlEscapedLazyText t
contentToText (ContentEntity e) = mconcat [fromByteString "&", fromLazyText e, fromByteString ";"]

type TAttribute = (TName, [Content])

foldAttrs :: [TAttribute] -> [Builder] -> [Builder]
foldAttrs attrs rest' =
    foldr go rest' attrs
  where
    go (key, val) rest =
        fromByteString " "
      : tnameToText key
      : fromByteString "=\""
      : foldr go' (fromByteString "\"" : rest) val
    go' (ContentText t) rest = fromHtmlEscapedLazyText t : rest
    go' (ContentEntity t) rest = fromByteString "&" : fromLazyText t : fromByteString ";" : rest

instance IsString TName where
    fromString = TName Nothing . T.pack
