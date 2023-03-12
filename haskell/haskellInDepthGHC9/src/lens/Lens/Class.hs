{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lens.Class
  ( Profunctor (..),
    Optic,
    Optic',
    LensLike,
    LensLike',
    Indexable (..),
    Indexed (..),
    IndexedLensLike,
    module X,
  )
where

import Lens.Monoids as X

-- note: Indexed function
newtype Indexed i a b where
  Indexed :: {runIndexed :: i -> a -> b} -> Indexed i a b
  deriving (Functor)

instance Applicative (Indexed i a) where
  pure = Indexed . const . const
  (Indexed f) <*> (Indexed x) = Indexed $ \i a -> f i a $ x i a

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (c -> d) -> p b c -> p b d
  rmap = dimap id

instance Profunctor (->) where
  dimap f g fn = g . fn . f

instance Profunctor (Indexed i) where
  dimap g h (Indexed f) = Indexed $ \i -> h . f i . g

class Profunctor p => Indexable i p where
  indexed :: p a b -> i -> a -> b

instance Indexable i (->) where
  indexed p _ = p

instance Indexable i (Indexed i) where
  indexed = runIndexed

type Optic p f s t a b = p a (f b) -> p s (f t)

type Optic' p f s a = Optic p f s s a a

type LensLike f s t a b = (a -> f b) -> s -> f t

type LensLike' f s a = LensLike f s s a a

type IndexedLensLike i f s t a b = forall p. Indexable i p => p a (f b) -> s -> f t
