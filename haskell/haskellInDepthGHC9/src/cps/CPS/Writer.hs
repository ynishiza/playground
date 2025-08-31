{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module CPS.Writer (CWriterT (..), runCWriter, module X) where

import CPS.Class as X
import Control.Arrow (first, second, (>>>))
import Control.Monad.Writer as X
import Control.Monad
import Data.Coerce
import Data.Functor.Identity

newtype CWriterT w m a = CWriterT {runCWriterT :: forall r. ((a, w) -> m r) -> m r}

type CWriter w = CWriterT w Identity

runCWriter :: CWriter w a -> ((a, w) -> r) -> r
runCWriter (CWriterT m) k = coerce $ m $ return . k

instance Monad m => Invertible (CWriterT w m) (WriterT w m) where
  from (CWriterT m) = WriterT $ m return
  to (WriterT n) = CWriterT (n >>=)

instance Functor (CWriterT w m) where
  fmap f (CWriterT m) = CWriterT $ \k -> m (first f >>> k)

instance Monoid w => Applicative (CWriterT w m) where
  pure a = CWriterT $ \k -> k (a, mempty)
  (<*>) = ap

instance Monoid w => Monad (CWriterT w m) where
  (CWriterT m) >>= h = CWriterT $ \k ->
    m (\(a, w) -> runCWriterT (h a) (second (w <>) >>> k))

instance Monoid w => MonadTrans (CWriterT w) where
  lift x = CWriterT $ \k -> x >>= ((,mempty) >>> k)

instance Monoid w => MonadWriter w (CWriterT w m) where
  tell w = CWriterT $ \k -> k ((), w)
  listen (CWriterT m) = CWriterT $ \k -> m $ \(a, w) -> k ((a, w), w)
  pass (CWriterT m) = CWriterT $ \k -> m $ \((a, f), w) -> k (a, f w)
