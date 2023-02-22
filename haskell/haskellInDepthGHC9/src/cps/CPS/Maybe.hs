{-# LANGUAGE RankNTypes #-}

module CPS.Maybe (CMaybe, CMaybeT (..), runCMaybe) where

import CPS.Class
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Functor.Identity

-- newtype CMaybe a = CMaybe {runCMaybe :: forall r. r -> (a -> r) -> r}

newtype CMaybeT m a = CMaybeT {runCMaybeT :: forall r. m r -> (a -> m r) -> m r}

type CMaybe = CMaybeT Identity

runCMaybe :: CMaybe a -> r -> (a -> r) -> r
runCMaybe (CMaybeT m) r k = coerce $ m (return r) (return . k)

instance Monad m => Invertible (CMaybeT m) (MaybeT m) where
  from (CMaybeT m) = MaybeT $ m (return Nothing) (return . Just)
  to (MaybeT n) = CMaybeT $ \r k -> n >>= maybe r k

instance Functor (CMaybeT m) where
  fmap f (CMaybeT m) = CMaybeT $ \r k -> m r (k . f)

instance Applicative (CMaybeT m) where
  pure a = CMaybeT $ \_ k -> k a
  (<*>) = ap

instance Monad (CMaybeT m) where
  (CMaybeT m) >>= h = CMaybeT $ \r k -> m r (\a -> runCMaybeT (h a) r k)

instance MonadTrans CMaybeT where
  lift x = CMaybeT $ \_ k -> x >>= k
