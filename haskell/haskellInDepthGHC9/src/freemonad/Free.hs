{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
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
    test,
    foldF,
    hoistF,
    MonadFree (..),
    Free (..),
    cutoff,
    List (..),
    ListF,
    liftNil,
    liftCons,
    liftList,
    execListF,
  )
where

import Control.Monad
import Control.Monad.Free (Free (..), MonadFree (..))
import Data.Foldable
import Data.Function
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
    step v m = cutoffAtN (wrap $ ((m + 1) &) <$> v) m
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

type List :: Type -> Type -> Type
data List t a where
  Nil :: a -> List t a
  Cons :: t -> a -> List t a
  deriving (Functor)

type ListF t = F (List t)

liftNil :: ListF t ()
liftNil = liftF $ Nil ()

liftCons :: t -> ListF t ()
liftCons t = liftF $ Cons t ()

liftList :: [t] -> ListF t ()
liftList = foldr (\t r -> liftCons t >> r) liftNil

execListF :: ListF t a -> ([t], a)
execListF (F f) = f ([],) m
  where
    m :: List t ([t], r) -> ([t], r)
    m (Nil (_, r)) = ([], r)
    m (Cons t (ts, a)) = (t : ts, a)

test :: IO ()
test = do
  let p0 :: ListF Int Int
      p0 = liftPure 100

      l0 :: ListF Int ()
      l0 = liftNil

      l1 :: ListF Int ()
      l1 = do
        liftCons 1
        liftCons 2
        liftCons 3

      l2 :: ListF Int ()
      l2 = do
        liftCons 1
        liftNil
        liftNil
        liftCons 2

      l3 :: ListF Int ()
      l3 = do
        liftCons 1
        liftList [100 .. 105]

  putStrLn $ "p0:" <> show (execListF p0)
  putStrLn $ "l0:" <> show (execListF l0)
  putStrLn $ "l1:" <> show (execListF l1)
  putStrLn $ "l2:" <> show (execListF l2)
  putStrLn $ "l3:" <> show (execListF l3)

  putStrLn "==================== cutoff ===================="
  traverse_
    ( \n ->
        putStrLn ("cutoff l3 " <> show n <> ":" <> show (execListF $ cutoff' n l3))
    )
    [1 .. 10]
  traverse_
    ( \n ->
        putStrLn ("cutoff l3 " <> show n <> ":" <> show (execListF $ cutoff n l3))
    )
    [1 .. 10]

  putStrLn $ "cutoff1 p0" <> show (execListF $ cutoff0 p0)
  putStrLn $ "cutoff1 l0" <> show (execListF $ cutoff0 l0)
  putStrLn $ "cutoff1 l1" <> show (execListF $ cutoff0 l1)

  pure ()
