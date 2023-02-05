{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

import Control.Monad
import Data.Kind

-- data MyCont r a where
--   MyCont :: {runCont :: (a -> r) -> r} -> MyCont r a

-- instance Functor (MyCont r) where
--   fmap :: (a -> b) -> MyCont r a -> MyCont r b
--   fmap f (MyCont c) = MyCont $ \fpure -> c (fpure . f)

-- instance Applicative (MyCont r) where
--   pure x = MyCont $ \fpure -> fpure x
--   (<*>) = ap

-- instance Monad (MyCont r) where
--   (>>=) :: MyCont r a -> (a -> MyCont r b) -> MyCont r b
--   (MyCont c) >>= k = MyCont $
--     \fpure -> c (\a -> runCont (k a) fpure)

data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a

data Fc f a where
  Fc :: {runFt :: forall r. (a -> r) -> (f r -> r) -> r} -> Fc f a

type MonadFree :: (Type -> Type) -> (Type -> Type) -> Constraint
class MonadFree f m where
  wrap :: f (m a) -> m a

instance Functor (Fc f) where
  fmap fn (Fc fld) = Fc $ \fpure ffree -> fld (fpure . fn) ffree

instance Applicative (Fc f) where
  pure = liftPure
  (<*>) = ap

instance Monad (Fc f) where
  (Fc fld) >>= k = Fc $ \fpure ffree -> fld (\a -> runFt (k a) fpure ffree) ffree

instance Functor f => MonadFree f (Fc f) where
  wrap :: f (Fc f a) -> Fc f a
  wrap f = Fc $ \fpure ffree -> ffree ((\(Fc fld) -> fld fpure ffree) <$> f)

liftPure :: a -> Fc f a
liftPure a = Fc $ \fpure _ -> fpure a

liftF :: Functor f => f a -> Fc f a
liftF f = Fc $ \fpure ffree -> ffree (fpure <$> f)

iter :: (f a -> a) -> Fc f a -> a
iter fn (Fc fld) = fld id fn

iterM :: Monad m => (f (m a) -> m a) -> Fc f a -> m a
iterM fn (Fc fld) = fld pure fn

retract :: Monad m => Fc m a -> m a
retract (Fc fld) = fld pure join

hoistF :: (forall x. f x -> g x) -> Fc f a -> Fc g a
hoistF mp (Fc fld) = Fc $ \fpure ffree -> fld fpure (ffree . mp)

foldF :: Monad m => (forall x. f x -> m x) -> Fc f a -> m a
foldF mp (Fc fld) = fld pure (join . mp)

toF :: Functor f => Free f a -> Fc f a
toF (Pure a) = pure a
toF (Free f) = wrap (toF <$> f)

cutoff :: Functor f => Int -> Fc f a -> Fc f (Maybe a)
cutoff n (Fc fld) 
  | n == 0 = undefined

