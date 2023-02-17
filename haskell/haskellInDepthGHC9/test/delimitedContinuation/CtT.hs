{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{- ORMOLU_DISABLE -}
module CtT (
  CtT(..),
  withCtT,
  retT,
  exitT,
  evalT,
  resetT,
  shiftT,
  (#>>=),
  (#>>),

  ct,
  Ct,
  runCt,
  eval,
  shift,
  ) where
{- ORMOLU_ENABLE -}

import Control.Monad (ap)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Coerce
import Data.Functor.Identity

newtype CtT r i m a = CtT {runCtT :: (a -> m i) -> m r}

instance Functor (CtT r i m) where
  fmap f = withCtT $ \k -> k . f

instance Applicative (CtT r r m) where
  pure = retT
  (<*>) = ap

instance Monad (CtT r r m) where
  (CtT c) >>= h = CtT $ \k -> c $ \a -> runCtT (h a) k

instance MonadTrans (CtT r r) where
  lift m = CtT (=<< m)

instance MonadIO m => MonadIO (CtT r r m) where
  liftIO = lift . liftIO

(#>>=) :: CtT r i m a -> (a -> CtT i j m b) -> CtT r j m b
(CtT c) #>>= f = CtT $ \k -> c (\a -> runCtT (f a) k)

(#>>) :: CtT r i m a -> CtT i j m b -> CtT r j m b
c #>> d = c #>>= const d

withCtT :: ((b -> m j) -> a -> m i) -> CtT r i m a -> CtT r j m b
withCtT f (CtT c) = CtT $ \k -> c (f k)

retT :: a -> CtT r r m a
retT a = CtT ($ a)

exitT :: Monad m => r -> CtT r i m a
exitT r = CtT $ const $ return r

evalT :: Monad m => CtT r a m a -> m r
evalT (CtT c) = c return

resetT :: Monad m => CtT a b m b -> CtT r r m a
resetT c = lift (evalT c)

shiftT :: Monad m => ((a -> m i) -> CtT r b m b) -> CtT r i m a
shiftT f = CtT $ evalT . f

type Ct r i = CtT r i Identity

runCt :: Ct r i a -> (a -> i) -> r
runCt (CtT c) = coerce . c . coerce

eval :: Ct r a a -> r
eval = coerce . evalT

ct :: ((a -> i) -> r) -> Ct r i a
ct f = CtT $ \k -> Identity $ f (coerce k)

shift :: ((a -> i) -> Ct r b b) -> Ct r i a
shift f = shiftT (f . coerce)
