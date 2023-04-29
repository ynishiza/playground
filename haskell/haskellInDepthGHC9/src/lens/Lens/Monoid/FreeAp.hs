{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Lens.Monoid.FreeAp
  ( FreeAp (..),
    fold,
    replaceWith,
    storeFoldr,
    storeTraverses,
  )
where

import Control.Arrow (Arrow (second))
import Data.Function
import GHC.Exts (IsList (..))

data FreeAp a o x where
  FPure :: x -> FreeAp a o x
  FFmap :: (x -> y) -> FreeAp a o x -> FreeAp a o y
  FAp :: FreeAp a o (x -> y) -> FreeAp a o x -> FreeAp a o y
  FStore :: a -> FreeAp a x x

instance Functor (FreeAp a o) where
  fmap = FFmap

instance Applicative (FreeAp a o) where
  pure = FPure
  f <*> x = FAp f x

fold :: FreeAp a a x -> x
fold (FStore x) = x
fold (FPure x) = x
fold (FFmap f x) = f (fold x)
fold (FAp f x) = fold f (fold x)

replaceWith :: (IsList l, Item l ~ a) => FreeAp a a x -> l -> (x, l)
replaceWith fap l =
  replaceWithList fap (toList l)
    & second fromList

replaceWithList :: FreeAp a a x -> [a] -> (x, [a])
replaceWithList c [] = (fold c, [])
replaceWithList (FStore _) (a: as) = (a, as)
replaceWithList (FPure v) as = (v, as)
replaceWithList (FFmap f x) as = (f x', as')
  where
    (x', as') = replaceWithList x as
replaceWithList (FAp f x) as = (f' x', as'')
  where
    (f', as') = replaceWithList f as
    (x', as'') = replaceWithList x as'

newtype FreeApDual x o a = FreeApDual (FreeAp a o x)

getFreeApDual :: FreeApDual x o a -> FreeAp a o x
getFreeApDual (FreeApDual ca) = ca

mapDualWith :: (FreeAp a o x -> FreeAp a' o' x') -> FreeApDual x o a -> FreeApDual x' o' a'
mapDualWith f (FreeApDual ca) = FreeApDual $ f ca

instance Foldable (FreeApDual x t) where
  foldr :: (a -> r -> r) -> r -> FreeApDual x t a -> r
  foldr reducer r (FreeApDual d) = case d of
    FPure _ -> r
    FStore a -> reducer a r
    FFmap _ x -> foldr reducer r (FreeApDual x)
    FAp f x -> foldr reducer (foldr reducer r (FreeApDual x)) (FreeApDual f)

instance Functor (FreeApDual x t) where
  fmap :: (a -> b) -> FreeApDual x t a -> FreeApDual x t b
  fmap g = mapDualWith $ \case
    FPure v -> FPure v
    FStore v -> FStore $ g v
    FFmap f x ->
      FFmap f (getFreeApDual $ g <$> FreeApDual x)
    FAp f x ->
      FAp
        (getFreeApDual $ g <$> FreeApDual f)
        (getFreeApDual $ g <$> FreeApDual x)

instance Traversable (FreeApDual x t) where
  traverse :: Applicative f => (a -> f b) -> FreeApDual x t a -> f (FreeApDual x t b)
  traverse g (FreeApDual d) = case d of
    FStore v -> FreeApDual . FStore <$> g v
    FPure x -> pure $ FreeApDual $ FPure x
    FFmap f x -> mapDualWith (FFmap f) <$> traverse g (FreeApDual x)
    FAp f x ->
      FreeApDual
        <$> ( FAp
                <$> (getFreeApDual <$> traverse g (FreeApDual f))
                <*> (getFreeApDual <$> traverse g (FreeApDual x))
            )

storeFoldr :: (a -> r -> r) -> r -> FreeAp a o x -> r
storeFoldr h r = foldr h r . FreeApDual

storeTraverses :: Applicative f => (a -> f b) -> FreeAp a o x -> f (FreeAp b o x)
storeTraverses h = (getFreeApDual <$>) . traverse h . FreeApDual
