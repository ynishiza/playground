{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Lens.Monoid
  ( Indexing (..),
    execIndexing,
    XFirst (..),
    getXFirst,
    TakeWhileApplicative (..),
  )
where

import Control.Arrow (first, second)
import Control.Category ((>>>))
import Data.Functor.Contravariant

-- note: Indexing monoid
newtype Indexing f a where
  Indexing :: {runIndexing :: Int -> (Int, f a)} -> Indexing f a
  deriving (Functor)

execIndexing :: Int -> Indexing f a -> f a
execIndexing n =
  runIndexing
    >>> ($ n)
    >>> snd

instance Applicative f => Applicative (Indexing f) where
  pure a = Indexing (,pure a)
  (Indexing f) <*> (Indexing x) = Indexing $ \i -> fn (f i) x
    where
      fn (j, b) y = second (b <*>) $ y j

instance Semigroup (f a) => Semigroup (Indexing f a) where
  (Indexing x) <> (Indexing y) = Indexing $ \i -> fn (x i) y
    where
      fn (j, b) z = second (b <>) $ z j

instance Monoid (f a) => Monoid (Indexing f a) where
  mempty = Indexing (,mempty)

data XFirst a where
  XFirst :: Maybe a -> XFirst a

getXFirst :: XFirst a -> Maybe a
getXFirst (XFirst x) = x

instance Semigroup (XFirst a) where
  (XFirst Nothing) <> x = x
  x <> _ = x

instance Monoid (XFirst a) where
  mempty = XFirst Nothing

data TakeWhileApplicative f a where
  TakeWhileApplicative :: {runTakeWhileApplicative :: Int -> (f a, Bool)} -> TakeWhileApplicative f a

instance Functor f => Functor (TakeWhileApplicative f) where
  fmap f (TakeWhileApplicative t) = TakeWhileApplicative $ first (f <$>) . t

instance Contravariant f => Contravariant (TakeWhileApplicative f) where
  contramap f (TakeWhileApplicative t) = TakeWhileApplicative $ first (contramap f) . t

instance (Contravariant f, Applicative f) => Applicative (TakeWhileApplicative f) where
  pure a = TakeWhileApplicative $ const (pure a, True)
  t1 <*> t2 = TakeWhileApplicative $ \i ->
    let (x1, takeX1) = runTakeWhileApplicative t1 i
     in if takeX1
          then first (x1 <*>) $ runTakeWhileApplicative t2 (i + 1)
          else (phantom x1, False)
