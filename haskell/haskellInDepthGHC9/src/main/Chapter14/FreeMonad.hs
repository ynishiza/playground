{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter14.FreeMonad
  ( FreePair (..),
    joinf,
  )
where

import Control.Monad
import Control.Monad.Free

data FreePair a where
  FPTerm :: a -> FreePair a
  FPPair :: FreePair a -> FreePair a -> FreePair a

data Pair a = MkPair a a

instance Functor FreePair where
  fmap f (FPTerm a) = FPTerm $ f a
  fmap f (FPPair x y) = FPPair (fmap f x) (fmap f y)

instance Applicative FreePair where
  pure = FPTerm
  (<*>) = ap

instance Monad FreePair where
  return = pure
  (FPTerm a) >>= k = k a
  (FPPair x y) >>= k = FPPair (x >>= k) (y >>= k)

instance MonadFree Pair FreePair where
  wrap :: Pair (FreePair a) -> FreePair a
  wrap (MkPair x y) = FPPair x y

joinf :: FreePair (FreePair a) -> FreePair a
joinf (FPTerm x) = x
joinf (FPPair x y) = FPPair (join x) (join y)
