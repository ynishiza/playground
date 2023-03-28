{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Free
  ( Free (..),
    MonadFree (..),
    liftF,
    retract,
    iter,
    foldFree,
    unfoldM,
    unfold,
    cutoff,
    hoistFree,
    iterM,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Functor.Classes
import Data.Typeable
import GHC.Generics

class MonadFree f m where
  wrap :: f (m a) -> m a

data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a
  deriving stock (Typeable, Generic)

instance Show1 f => Show1 (Free f) where
  liftShowsPrec s _ i (Pure a) = ("Pure " <>) . s i a
  liftShowsPrec s t i (Free f) =
    ("Free " <>)
      . showParen
        True
        ( liftShowsPrec
            (\j a -> liftShowsPrec s t j a)
            (\l -> foldMap (liftShowsPrec s t i) l)
            i
            f
        )

instance MonadState s f => MonadState s (Free f) where
  get = lift get
  put = lift . put

instance MonadError e f => MonadError e (Free f) where
  throwError = lift . throwError
  catchError (Pure a) _ = Pure a
  catchError (Free f) catch = Free $ catchError f (pure . catch)

instance Foldable f => Foldable (Free f) where
  foldr :: (a -> r -> r) -> r -> Free f a -> r
  foldr g r (Pure a) = g a r
  foldr g r (Free f) = foldr (flip $ foldr g) r f

instance Traversable f => Traversable (Free f) where
  traverse :: Applicative g => (a -> g b) -> Free f a -> g (Free f b)
  traverse g (Pure a) = Pure <$> g a
  traverse g (Free f) = Free <$> traverse (traverse g) f

instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure $ g a
  fmap g (Free f) = Free $ (g <$>) <$> f

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (Pure a) >>= k = k a
  (Free f) >>= k = Free $ (>>= k) <$> f

instance MonadFail f => MonadFail (Free f) where
  fail = lift . fail

instance MonadFree f (Free f) where
  wrap :: f (Free f a) -> Free f a
  wrap = Free

instance MonadTrans Free where
  lift :: Monad f => f a -> Free f a
  lift = liftF

retract :: Monad f => Free f a -> f a
retract (Pure a) = pure a
retract (Free f) = f >>= retract

liftF :: Functor f => f a -> Free f a
liftF = Free . (Pure <$>)

iter :: Functor f => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter g (Free f) = g $ iter g <$> f

iterM :: (Functor f, Monad m) => (f (m a) -> m a) -> Free f a -> m a
iterM _ (Pure a) = pure a
iterM g (Free f) = g $ iterM g <$> f

hoistFree :: Functor g => (forall x. f x -> g x) -> Free f a -> Free g a
hoistFree _ (Pure a) = Pure a
hoistFree g (Free f) = Free $ hoistFree g <$> g f

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = pure a
foldFree g (Free f) = g f >>= foldFree g

cutoff :: Functor f => Int -> Free f a -> Free f (Maybe a)
cutoff n x
  | n > 0, (Pure a) <- x = Pure $ Just a
  | n > 0, (Free f) <- x = Free $ cutoff (n - 1) <$> f
  | otherwise = Pure Nothing

unfold :: Functor f => (b -> Either a (f b)) -> b -> Free f a
unfold g r = case g r of
  Left a -> Pure a
  Right f -> Free $ unfold g <$> f

unfoldM :: (Traversable f, Monad m) => (b -> m (Either a (f b))) -> b -> m (Free f a)
unfoldM g r =
  g r >>= \case
    (Left a) -> pure $ Pure a
    (Right f) -> Free <$> mapM (unfoldM g) f
