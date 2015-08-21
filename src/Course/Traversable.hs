{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Id
import Course.Optional
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.List

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse Id x ≅ Id x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- | Traversable instance for List.
--
-- >>> let xs = listh [1,2,3]
--
-- The law of identity
-- >>> traverse Id xs == Id xs
-- True
--
-- >>> let f = (+1)
-- >>> let g = (+1)
--
-- The law of naturality
-- >>> (f <$>) <$> (traverse (Id . g) xs)
-- Id [3,4,5]
-- >>> traverse (Id . f . g) xs 
-- Id [3,4,5]
--
-- The law of composition
-- traverse ((g <$>) . f) == (traverse g <$>) . traverse f
instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f = foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)
