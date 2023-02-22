{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module CPS.Function (Cfn (..)) where

import CPS.Class
import Control.Monad

newtype Cfn t a = Cfn {runCfn :: forall r. (a -> r) -> t -> r}

instance Invertible (Cfn t) ((->) t) where
  from (Cfn m) = m id
  to f = Cfn $ \k t -> k (f t)

instance Functor (Cfn t) where
  fmap f (Cfn m) = Cfn $ \k t -> m (k . f) t

instance Applicative (Cfn t) where
  pure a = Cfn $ \k _ -> k a
  (<*>) = ap

instance Monad (Cfn t) where
  (>>=) :: Cfn t a -> (a -> Cfn t b) -> Cfn t b
  (Cfn m) >>= h = Cfn $ \k t -> m (\a -> runCfn (h a) k t) t
