module Text.HTML.TagStream.Types where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Arrow ((***))
import Data.Monoid (mappend, mconcat)
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse), sequenceA)

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
    fmap f t = case t of
        (TagOpen x pairs b) -> TagOpen (f x) (map (f *** f) pairs) b
        (TagClose x)        -> TagClose (f x)
        (Text x)            -> Text (f x)
        (Comment x)         -> Comment (f x)
        (Special x y)       -> Special (f x) (f y)
        (Incomplete x)      -> Incomplete (f x)

instance Foldable Token' where
    foldMap f t = case t of
        (TagOpen x pairs _) -> f x `mappend` mconcat (map (\(a1, a2) -> f a1 `mappend` f a2) pairs)
        (TagClose x)        -> f x
        (Text x)            -> f x
        (Comment x)         -> f x
        (Special x y)       -> f x `mappend` f y
        (Incomplete x)      -> f x

instance Traversable Token' where
    traverse f t = case t of
        (TagOpen x pairs b) -> TagOpen <$> f x
                                       <*> sequenceA (map (\(a1, a2) -> (,) <$> f a1 <*> f a2) pairs)
                                       <*> pure b
        (TagClose x)        -> TagClose <$> f x
        (Text x)            -> Text <$> f x
        (Comment x)         -> Comment <$> f x
        (Special x y)       -> Special <$> f x <*> f y
        (Incomplete x)      -> Incomplete <$> f x

