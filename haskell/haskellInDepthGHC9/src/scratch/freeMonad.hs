{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad

data MyCont r a where
  MyCont :: {runCont :: (a -> r) -> r} -> MyCont r a

instance Functor (MyCont r) where
  fmap :: (a -> b) -> MyCont r a -> MyCont r b
  fmap f (MyCont c) = MyCont $ \mkPure -> c (mkPure . f)

instance Applicative (MyCont r) where
  pure x = MyCont $ \mkPure -> mkPure x
  (<*>) = ap

instance Monad (MyCont r) where
  (>>=) :: MyCont r a -> (a -> MyCont r b) -> MyCont r b
  (MyCont c) >>= k = MyCont $
    \mkPure -> c (\a -> runCont (k a) mkPure)

data FreeT f m a where
  FreeT :: {runFt :: forall r. (a -> m r) -> (forall x. (x -> m r) -> f x -> m r) -> m r} -> FreeT f m a

type Ftp m a r = a -> m r

type Ftf f m a r = forall x. (x -> m r) -> f x -> m r

liftConst :: a -> FreeT f m a
liftConst a = FreeT $ \mkPure _ -> mkPure a

liftF :: f a -> FreeT f m a
liftF f = FreeT $ \mkPure mkFree -> mkFree mkPure f

instance Functor (FreeT f m) where
  fmap fn (FreeT f) = FreeT $ \mkPure mkFree -> f (mkPure . fn) mkFree

instance Applicative (FreeT f m) where
  pure = liftConst
  (<*>) = ap

instance Monad (FreeT f m) where
  (FreeT f) >>= k = FreeT $ \mkPure mkFree -> f (\a -> runFt (k a) mkPure mkFree) mkFree
