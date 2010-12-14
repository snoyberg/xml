{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Enumerator.Token
    ( tokenToBuilder
    , TName (..)
    , Token (..)
    , TAttribute
    , NSLevel (..)
    ) where

import Data.XML.Types (Instruction (..), Content (..), ExternalID (..))
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.String (IsString (fromString))
import Blaze.ByteString.Builder (Builder, fromByteString, writeByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)
import Blaze.ByteString.Builder.Char.Utf8 (writeChar, fromLazyText)
import Data.Monoid (mconcat, mempty)
import Data.ByteString.Char8 ()
import Data.Map (Map)

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
    , fromLazyText e
    , fromByteString ";"
    ]

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
    go' (ContentText t) rest =
        fromWriteList h (T.unpack t) : rest
      where
        h '<' = writeByteString "&lt;"
        h '>' = writeByteString "&gt;"
        h '&' = writeByteString "&amp;"
        h '"' = writeByteString "&quot;"
        -- Not escaping single quotes, since our attributes are always double
        -- quoted
        h c   = writeChar c
    go' (ContentEntity t) rest =
        fromByteString "&" : fromLazyText t : fromByteString ";" : rest

instance IsString TName where
    fromString = TName Nothing . T.pack

data NSLevel = NSLevel
    { defaultNS :: Maybe Text
    , prefixes :: Map Text Text
    }
    deriving Show
