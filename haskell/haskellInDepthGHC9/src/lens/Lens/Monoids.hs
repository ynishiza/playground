{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Lens.Monoids
  ( Indexing (..),
    XFirst (..),
    getXFirst,
    TakeWhileR (..),
    -- withTk,
  )
where

import Control.Arrow (second)

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

data TakeWhileR a where
  TakeWhileR :: {runTakeWhileR :: Int -> Maybe a} -> TakeWhileR a

instance Semigroup a => Semigroup (TakeWhileR a) where
  -- IMPORTANT: don't pattern match TakeWhileR to allow lazy evaluation of <>
  -- i.e. don't do
  --      (TakeWhileR t1) <> (TakeWhileR t2)
  t1 <> t2 = TakeWhileR $ \i -> case runTakeWhileR t1 i of
    (Just x) -> case runTakeWhileR t2 (i + 1) of
      (Just y) -> Just $ x <> y
      _ -> Just x
    Nothing -> Nothing

instance Semigroup a => Monoid (TakeWhileR a) where
  mempty = TakeWhileR $ const Nothing
