{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE RankNTypes #-}

import Control.Monad

data MyCont r a where
  MkCont :: { runCont :: (a -> r) -> r } -> MyCont r a

instance Functor (MyCont r) where
  fmap :: (a -> b) -> MyCont r a -> MyCont r b
  fmap f (MkCont c) = MkCont $ \cb -> c (cb . f)

instance Applicative (MyCont r) where
  pure x = MkCont $ \cb -> cb x
  (<*>) = ap

instance Monad (MyCont r) where
  (>>=) :: MyCont r a -> (a -> MyCont r b) -> MyCont r b
  (MkCont c) >>= k = MkCont $ 
    \cb -> c (\a -> runCont (k a) cb)


data Ft f m a where
  MkFree :: { runFt :: forall r. (a -> m r) -> (forall x. (x -> m r) -> f x -> m r) -> m r } -> Ft f m a

type Ftp m a r = a -> m r
type Ftf f m a r = forall x. (x -> m r) -> f x -> m r

liftConst :: a -> Ft f m a
liftConst a = MkFree $ \cb _ -> cb a

liftF :: Functor f => f a ->  Ft f m a
liftF f = MkFree $ \cba cbf -> cbf cba f

instance Functor (Ft f m) where
  fmap fn (MkFree f) = MkFree $ \cba cbf -> f (cba . fn) cbf
  
instance Applicative (Ft f m) where
  pure = liftConst
  (<*>) = ap

instance Monad (Ft f m) where
  (MkFree f) >>= k = MkFree $ \cba cbf -> f (\a -> runFt (k a) cba cbf) cbf
