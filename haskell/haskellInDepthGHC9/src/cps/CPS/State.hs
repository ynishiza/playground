{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module CPS.State (CStateT (..), module X, runCState) where

import CPS.Class
import Control.Arrow (first, (>>>))
import Control.Monad.State as X
import Control.Monad
import Data.Coerce
import Data.Functor.Identity

newtype CStateT s m a where
  CStateT :: {runCStateT :: forall r. ((a, s) -> m r) -> s -> m r} -> CStateT s m a

type CState s = CStateT s Identity

runCState :: CState s a -> ((a, s) -> r) -> s -> r
runCState (CStateT m) k s = coerce $ m (return . k) s

instance Monad m => Invertible (CStateT s m) (StateT s m) where
  from :: CStateT s m a -> StateT s m a
  from (CStateT m) = StateT $ \s -> m return s
  to :: StateT s m a -> CStateT s m a
  to (StateT n) = CStateT $ \k s -> n s >>= k

instance Functor (CStateT s m) where
  fmap f (CStateT m) = CStateT $ \k s -> m (k . first f) s

instance Applicative (CStateT s m) where
  pure a = CStateT $ \k s -> k (a, s)
  (<*>) = ap

instance Monad (CStateT s m) where
  (CStateT m) >>= h = CStateT $ \k s ->
    m (\(a, s') -> runCStateT (h a) k s') s

instance MonadTrans (CStateT s) where
  lift x = CStateT $ \k s -> x >>= ((,s) >>> k)

instance Monad m => MonadState s (CStateT s m) where
  get = CStateT $ \k s -> k (s, s)
  put s = CStateT $ \k _ -> k ((), s)
