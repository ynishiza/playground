{-# LANGUAGE FlexibleInstances #-}

module Lens.Class
  ( Profunctor (..),
    Indexable (..),
    Optic,
    Optic',
  )
where

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (c -> d) -> p b c -> p b d
  rmap = dimap id

class Profunctor p => Indexable i p where
  indexed :: p a b -> i -> a -> b

instance Profunctor (->) where
  dimap f g fn = g . fn . f

instance Indexable i (->) where
  indexed p _ = p

type Optic p f s t a b = p a (f b) -> p s (f t)
type Optic' p f s a = Optic p f s s a a
