{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lens.Class
  ( Profunctor (..),
    NormalProfunctor (..),
    Choice (..),
    Indexable (..),
    Indexed (..),
    ToProfunctor(..),
    module X,
  )
where

import Lens.Monoid as X
import Lens.TraverseMonoid as X

-- note: Indexed function
newtype Indexed i a b where
  Indexed :: {runIndexed :: i -> a -> b} -> Indexed i a b
  deriving (Functor)

instance Applicative (Indexed i a) where
  pure = Indexed . const . const
  (Indexed f) <*> (Indexed x) = Indexed $ \i a -> f i a $ x i a

-- ==================== Profunctor ====================

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (c -> d) -> p b c -> p b d
  rmap = dimap id

class Profunctor p => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

class Profunctor p => NormalProfunctor p where
  normalDimap :: (a -> b) -> (a -> c -> d) -> p b c -> p a d

class Profunctor p => ToProfunctor p where
  toProfunctor :: (a -> b) -> p a b

instance Profunctor (->) where
  dimap f g fn = g . fn . f

instance Profunctor (Indexed i) where
  dimap g h (Indexed f) = Indexed $ \i -> h . f i . g

instance NormalProfunctor (->) where
  normalDimap f g fn a = g a $ fn $ f a

instance NormalProfunctor (Indexed i) where
  normalDimap f g (Indexed fn) = Indexed $ \i a -> g a $ fn i $ f a

instance ToProfunctor (->) where
  toProfunctor f = f

instance ToProfunctor (Indexed i) where
  toProfunctor = Indexed . const

instance Choice (->) where
  left' f (Left a) = Left $ f a
  left' _ (Right c) = Right c
  right' f (Right a) = Right $ f a
  right' _ (Left c) = Left c

instance Choice (Indexed i) where
  left' (Indexed f) = Indexed $ \i x -> case x of
    (Left a) -> Left $ f i a
    (Right c) -> Right c
  right' (Indexed f) = Indexed $ \i x -> case x of
    (Right a) -> Right $ f i a
    (Left c) -> Left c

class Profunctor p => Indexable i p where
  indexed :: p a b -> i -> a -> b

instance Indexable i (->) where
  indexed p _ = p

instance Indexable i (Indexed i) where
  indexed = runIndexed
