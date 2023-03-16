{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Lens.Monoids
  ( Indexing (..),
    XFirst (..),
    getXFirst,
    TakeWhile1 (..),
    TakeWhileApplicative (..),
    DroppingWhileR (..),
    DroppingWhileApplicative (..),
  )
where

import Control.Arrow (first, second)

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

data TW f a where
  TW :: {runTW :: Int -> (Bool, f a)} -> TW f a

instance Functor f => Functor (TW f) where
  fmap f (TW x) = TW $ second (f <$>) . x

instance Applicative f => Applicative (TW f) where
  pure a = TW $ const (True, pure a)

data TakeWhileApplicative f a where
  TakeWhileApplicative :: {runTakeWhileApplicative :: Int -> (Int -> f a) -> f a} -> TakeWhileApplicative f a

instance Applicative f => Semigroup (TakeWhileApplicative f a) where
  t1 <> t2 = TakeWhileApplicative $ \i k -> runTakeWhileApplicative t1 i (\j -> runTakeWhileApplicative t2 j k)

instance Applicative f => Monoid (TakeWhileApplicative f a) where
  mempty = TakeWhileApplicative $ \i k -> k i

data DroppingWhileApplicative f a where
  DroppingWhileApplicative :: {runDroppingWhileApplicative :: Int -> f a -> f a} -> DroppingWhileApplicative f a

instance Applicative f => Semigroup (DroppingWhileApplicative f a) where
  d1 <> d2 = DroppingWhileApplicative $ \i v -> runDroppingWhileApplicative d2 (i + 1) (runDroppingWhileApplicative d1 i v)

instance Applicative f => Monoid (DroppingWhileApplicative f a) where
  mempty = DroppingWhileApplicative $ \_ v -> v

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

-- ==================== OLD ====================
--
data TakeWhile1 a where
  TakeWhile1 :: {runTakeWhile1 :: Int -> Maybe a} -> TakeWhile1 a

instance Semigroup a => Semigroup (TakeWhile1 a) where
  t1 <> t2 = TakeWhile1 $ \i -> case runTakeWhile1 t1 i of
    (Just x) -> case runTakeWhile1 t2 (i + 1) of
      (Just y) -> Just $ x <> y
      _ -> Just x
    Nothing -> Nothing

instance Semigroup a => Monoid (TakeWhile1 a) where
  mempty = TakeWhile1 $ const Nothing
