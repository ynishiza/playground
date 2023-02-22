{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module CPS.Free (CFree (..)) where

import CPS.Class
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Free

newtype CFree f a = CFree {runCFree :: forall r. (a -> r) -> (f r -> r) -> r}

runCFreeWith :: (a -> r) -> (f r -> r) -> CFree f a -> r
runCFreeWith k kf (CFree m) = m k kf

instance Functor (CFree f) where
  fmap f (CFree m) = CFree $ \ka kf -> m (ka . f) kf

instance Applicative (CFree f) where
  pure a = CFree $ \ka _ -> ka a
  (<*>) = ap

instance Monad (CFree f) where
  (CFree m) >>= h = CFree $ \kb kf -> m (\a -> runCFree (h a) kb kf) kf

instance Functor f => Invertible (CFree f) (Free f) where
  from :: CFree f a -> Free f a
  from (CFree m) = m Pure Free
  to :: forall a. Free f a -> CFree f a
  to (Pure a) = CFree $ \ka _ -> ka a
  to (Free f) = CFree $ \ka kf -> kf (runCFreeWith ka kf . to <$> f)

instance MonadTrans CFree  where
  lift m = CFree $ \k kf -> kf (k <$> m)

instance Functor f => MonadFree f (CFree f) where
  wrap m = CFree $ \k kf -> kf (runCFreeWith k kf <$> m)

