module Text.HTML.TagStream.Types where

import Control.Arrow ((***))

type Attr' s = (s, s)

data Token' s = TagOpen s [Attr' s] Bool
              | TagClose s
              | Text s
              | Comment s
              | Special s s
              | Incomplete s
    deriving (Eq, Show)

data TagType = TagTypeClose
             | TagTypeSpecial
             | TagTypeNormal

instance Functor Token' where
    fmap f (TagOpen x pairs b) = TagOpen (f x) (map (f *** f) pairs) b
    fmap f (TagClose x) = TagClose (f x)
    fmap f (Text x) = Text (f x)
    fmap f (Comment x) = Comment (f x)
    fmap f (Special x y) = Special (f x) (f y)
    fmap f (Incomplete x) = Incomplete (f x)
