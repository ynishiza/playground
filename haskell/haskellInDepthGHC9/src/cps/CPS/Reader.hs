{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module CPS.Reader (CReaderT (..), runCReader, module X) where

import CPS.Class as X
import Control.Monad.Reader as X
import Data.Coerce
import Control.Monad
import Data.Functor.Identity

newtype CReaderT r m a = CReaderT {runCReaderT :: forall s. r -> (a -> m s) -> m s}

type CReader r = CReaderT r Identity

runCReader :: CReader r a -> r -> (a -> s) -> s
runCReader (CReaderT m) r k = coerce $ m r (return . k)

instance Monad m => Invertible (CReaderT r m) (ReaderT r m) where
  from (CReaderT m) = ReaderT $ \r -> m r return
  to (ReaderT n) = CReaderT $ \r k -> n r >>= k

instance Functor (CReaderT r m) where
  fmap f (CReaderT m) = CReaderT $ \r k -> m r (k . f)

instance Applicative (CReaderT r m) where
  pure a = CReaderT $ \_ k -> k a
  (<*>) = ap

instance Monad (CReaderT r m) where
  (CReaderT m) >>= h = CReaderT $ \r k -> m r $ \a -> runCReaderT (h a) r k

instance MonadReader r (CReaderT r m) where
  ask = CReaderT $ \r k -> k r
  local f (CReaderT m) = CReaderT $ \r k -> m (f r) k

instance MonadTrans (CReaderT r) where
  lift x = CReaderT $ \_ k -> x >>= k
