{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# HLINT ignore "Redundant pure" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Free
  ( F (..),
    runWith,
    liftPure,
    liftF,
    iter,
    iterM,
    retract,
    foldF,
    hoistF,
    MonadFree (..),
    Free (..),
    cutoff,
    cutoff',
    cutoff0,
    toF,
    fromF,
  )
where

import Control.Monad
import Control.Monad.Free (Free (..), MonadFree (..))
import Data.Kind

type F :: (Type -> Type) -> Type -> Type
data F f a where
  F :: {runF :: forall r. (a -> r) -> (f r -> r) -> r} -> F f a

runWith :: (a -> r) -> (f r -> r) -> F f a -> r
runWith kpure kfree (F f) = f kpure kfree

instance Functor (F f) where
  fmap fn (F f) = F $ \kpure kfree -> f (kpure . fn) kfree

instance Applicative (F f) where
  pure = liftPure
  (<*>) = ap

instance Monad (F f) where
  (>>=) :: F f a -> (a -> F f b) -> F f b
  (F f) >>= k = F $ \kpure kfree -> f (\a -> runF (k a) kpure kfree) kfree

instance Functor f => MonadFree f (F f) where
  wrap :: f (F f a) -> F f a
  wrap f = F $ \kpure kfree -> kfree (runWith kpure kfree <$> f)

liftPure :: a -> F f a
liftPure a = F $ \kpure _ -> kpure a

liftF :: Functor f => f a -> F f a
liftF f = F $ \kpure kfree -> kfree (kpure <$> f)

iter :: (f a -> a) -> F f a -> a
iter kfree (F f) = f id kfree

iterM :: Monad m => (f (m a) -> m a) -> F f a -> m a
iterM kfree (F f) = f pure kfree

retract :: Monad m => F m a -> m a
retract (F f) = f pure join

hoistF :: (forall x. f x -> g x) -> F f a -> F g a
hoistF fn (F f) = F $ \kpure kfree -> f kpure (kfree . fn)

foldF :: Monad m => (forall x. f x -> m x) -> F f a -> m a
foldF fn (F f) = f pure (join . fn)

toF :: Functor f => Free f a -> F f a
toF (Pure a) = liftPure a
toF (Free f) = wrap $ toF <$> f

fromF :: F f a -> Free f a
fromF (F f) = f Pure Free

cutoff :: forall f a. Functor f => Int -> F f a -> F f (Maybe a)
cutoff n (F f) = f (cutoffAtN . liftPure . Just) step 0
  where
    step :: f (Int -> F f (Maybe a)) -> Int -> F f (Maybe a)
    step value m = cutoffAtN resolved m
      where resolved = wrap $ ($ (m + 1)) <$> value
    cutoffAtN :: F f (Maybe a) -> Int -> F f (Maybe a)
    cutoffAtN g m = if m < n then g else liftPure Nothing

cutoff' :: Functor f => Int -> F f a -> F f (Maybe a)
cutoff' n = toF . cutoffBase n . fromF

cutoff0 :: F f a -> F f (Maybe a)
cutoff0 (F f) = f (liftPure . Just) (const (liftPure Nothing))

cutoffBase :: Functor f => Int -> Free f a -> Free f (Maybe a)
cutoffBase 0 _ = Pure Nothing
cutoffBase _ (Pure a) = Pure (Just a)
cutoffBase n (Free f) = Free $ cutoffBase (n - 1) <$> f
