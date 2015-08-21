{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Id
import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a)) deriving (Show)

-- | Implement a Functor instance for Compose
-- >>> (+1) <$> Compose (Id (Id 1)) 
-- Compose (Id (Id 2))
--
-- >>> (+1) <$> Compose [[1]] 
-- Compose [[2]]
instance (Functor f, Functor g) => Functor (Compose f g) where
  f <$> (Compose g) = Compose $ (f <$>) <$> g

-- | Implement the (<*>) function for an Apply instance for Compose
-- >>> pure (+1) <*> Compose (Id (Id 1))
-- Compose (Id (Id 2))
--
-- >>> pure (+1) <*> Compose [[1]] 
-- Compose [[2]]
instance (Apply f, Apply g) => Apply (Compose f g) where
  (Compose f) <*> (Compose a) = Compose $ lift2 (<*>) f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure

instance (Bind f, Bind g) => Bind (Compose f g) where
-- Implement the (=<<) function for a Bind instance for Compose
  f =<< (Compose a) = error "can't achieve this one"
