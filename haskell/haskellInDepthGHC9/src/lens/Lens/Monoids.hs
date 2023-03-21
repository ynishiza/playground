{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Lens.Monoids
  ( Indexing (..),
    XFirst (..),
    getXFirst,
    TakeWhileApplicative (..),
    DroppingWhileR (..),
    DroppingWhileApplicative (..),
  )
where

import Control.Arrow (first, second)
import Data.Functor.Contravariant

-- note: Indexing monoid
newtype Indexing f a where
  Indexing :: {runIndexing :: Int -> (Int, f a)} -> Indexing f a
  deriving (Functor)

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

data DroppingWhileApplicative f a where
  DroppingWhileApplicative :: {runDroppingWhileApplicative :: (Int, Bool) -> (f a, Bool)} -> DroppingWhileApplicative f a

instance Functor f => Functor (DroppingWhileApplicative f) where
  fmap f (DroppingWhileApplicative dropA) = DroppingWhileApplicative $ first (f <$>) . dropA

instance Contravariant f => Contravariant (DroppingWhileApplicative f) where
  contramap f (DroppingWhileApplicative dropA) = DroppingWhileApplicative $ first (contramap f) . dropA

instance Applicative f => Applicative (DroppingWhileApplicative f) where
  pure a = DroppingWhileApplicative $ \(_, dropping) -> (pure a, dropping)
  dropF <*> dropA = DroppingWhileApplicative $ \(i, dropping) ->
    let (f, d1) = runDroppingWhileApplicative dropF (i, dropping)
     in first (f <*>) $ runDroppingWhileApplicative dropA (i + 1, d1)

data DroppingWhileR a where
  DroppingWhileR :: {runDroppingWhileR :: (Int, Bool) -> (a, Bool)} -> DroppingWhileR a

instance Semigroup a => Semigroup (DroppingWhileR a) where
  d1 <> d2 = DroppingWhileR $ \(i, isDropping) ->
    let (x1, dropX1) = runDroppingWhileR d1 (i, isDropping)
     in case (isDropping, dropX1) of
          -- case: drop x1
          (True, True) -> runDroppingWhileR d2 (i + 1, True)
          -- case: take x1
          _ -> first (x1 <>) $ runDroppingWhileR d2 (i + 1, False)

instance Monoid a => Monoid (DroppingWhileR a) where
  mempty = DroppingWhileR $ const (mempty, True)

