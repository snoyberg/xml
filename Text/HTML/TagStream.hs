module Text.HTML.TagStream
  ( tokenStream
  , Token
  , Token'(..)
  , Attr
  , Attr'
  , showToken
  , encode
  , encodeHL
  , decode
  ) where

import Text.HTML.TagStream.Types
import Text.HTML.TagStream.Stream
import Text.HTML.TagStream.Parser
