{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}

{- ORMOLU_DISABLE -}
module Lens.Class
  ( 
    Profunctor (..),
    ProfunctorChoice (..),
    Indexable (..),
    ProfunctorArrow(..),
    ProfunctorRepresentation(..),
    ProfunctorNormal,
    strong,
    uncurry',

    Distributable(..),
    Settable(..),
  )
where
{- ORMOLU_ENABLE -}

import Control.Arrow (Arrow (..), Kleisli (..), (>>>))
import Data.Coerce
import Data.Function ((&))
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Tuple (swap)

class Distributable t where
  {-# MINIMAL distribute | collect #-}
  distribute :: Functor f => f (t a) -> t (f a)
  distribute = collect id
  collect :: Functor f => (a -> t b) -> f a -> t (f b)
  collect f = distribute . (f <$>)

instance Distributable Identity where
  distribute = Identity . (runIdentity <$>)

instance Distributable ((->) a) where
  distribute v x = ($ x) <$> v

class (Applicative f, Traversable f) => Settable f where
  {-# MINIMAL untainted, tainted #-}
  untainted :: f a -> a
  tainted :: a -> f a

  -- untainedDot f = untainted . f
  -- taintedDot f = tainted . f
  untaintedDot :: Profunctor p => p a (f b) -> p a b
  untaintedDot = rmap untainted
  taintedDot :: Profunctor p => p a b -> p a (f b)
  taintedDot = rmap tainted

instance Settable Identity where
  untainted = coerce
  tainted = coerce

-- ==================== Profunctor ====================

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap l r = lmap l >>> rmap r
  lmap :: (a -> b) -> p b c -> p a c
  lmap l = dimap l id
  rmap :: (c -> d) -> p b c -> p b d
  rmap = dimap id

class Profunctor p => ProfunctorChoice p where
  {-# MINIMAL left' | right' #-}
  left' :: p a b -> p (Either a c) (Either b c)
  left' k =
    right' k
      & dimap (either Right Left) (either Right Left)
  right' :: p a b -> p (Either c a) (Either c b)
  right' k =
    left' k
      & dimap (either Right Left) (either Right Left)

class Profunctor p => ProfunctorArrow p where
  {-# MINIMAL arr', (first' | second') #-}
  arr' :: (a -> b) -> p a b
  first' :: p a b -> p (a, c) (b, c)
  first' p =
    second' p
      & dimap swap swap

  second' :: p a b -> p (c, a) (c, b)
  second' p =
    first' p
      & dimap swap swap

strong :: ProfunctorArrow p => (a -> b -> c) -> p a b -> p a c
strong f pab =
  second' pab
    & dimap dup (uncurry f)
  where
    dup x = (x, x)

uncurry' :: ProfunctorArrow p => p a (b -> c) -> p (a, b) c
uncurry' p =
  first' p
    & rmap (\(f, b) -> f b)

class Profunctor p => Indexable i p where
  indexed :: p a b -> i -> a -> b

class (ProfunctorArrow p, Functor (Rep p)) => ProfunctorRepresentation p where
  type Rep p :: Type -> Type
  toRep :: p a b -> a -> Rep p b
  fromRep :: (a -> Rep p b) -> p a b

type ProfunctorNormal p = (Monad (Rep p), Distributable (Rep p), ProfunctorRepresentation p)

instance Profunctor (->) where
  dimap f g fn = g . fn . f

instance ProfunctorArrow (->) where
  arr' = id
  first' = first

instance ProfunctorChoice (->) where
  left' f (Left a) = Left $ f a
  left' _ (Right c) = Right c

instance Indexable i (->) where
  indexed p _ = p

instance ProfunctorRepresentation (->) where
  type Rep (->) = Identity
  toRep p = Identity . p
  fromRep f = runIdentity . f

instance Functor m => Profunctor (Kleisli m) where
  dimap l r (Kleisli f) = Kleisli $ l >>> f >>> (r <$>)

instance Applicative m => ProfunctorArrow (Kleisli m) where
  arr' f = Kleisli $ pure . f
  first' (Kleisli f) = Kleisli $ \(a, b) -> (,b) <$> f a

instance Applicative m => ProfunctorChoice (Kleisli m) where
  left' (Kleisli f) = Kleisli $ \case
    (Left a) -> Left <$> f a
    (Right b) -> pure $ Right b
