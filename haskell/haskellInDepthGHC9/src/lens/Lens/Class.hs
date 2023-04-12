{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- ORMOLU_DISABLE -}
module Lens.Class
  ( 
    Profunctor (..),
    ProfunctorChoice (..),
    Indexable (..),
    ProfunctorArrow(..),
    strong,
    uncurry',

    Settable(..),
    module X,
  )
where
{- ORMOLU_ENABLE -}

import Control.Arrow (Arrow (..), Kleisli (..), (>>>))
import Data.Coerce
import Data.Function ((&))
import Data.Functor.Identity
import Data.Tuple (swap)
import Lens.Monoid as X
import Lens.TraverseMonoid as X

-- ==================== Profunctor ====================

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap l r = lmap l >>> rmap r
  lmap :: (a -> b) -> p b c -> p a c
  lmap l = dimap l id
  rmap :: (c -> d) -> p b c -> p b d
  rmap = dimap id

class Profunctor p => ProfunctorChoice p where
  left' :: p a b -> p (Either a c) (Either b c)
  left' k =
    right' k
      & dimap (either Right Left) (either Right Left)
  right' :: p a b -> p (Either c a) (Either c b)
  right' k =
    left' k
      & dimap (either Right Left) (either Right Left)

class Profunctor p => ProfunctorArrow p where
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

instance Functor m => Profunctor (Kleisli m) where
  dimap l r (Kleisli f) = Kleisli $ l >>> f >>> (r <$>)

instance Applicative m => ProfunctorArrow (Kleisli m) where
  arr' f = Kleisli $ f >>> pure
  first' (Kleisli f) = Kleisli $ \(a, b) -> (,b) <$> f a

instance Applicative m => ProfunctorChoice (Kleisli m) where
  left' (Kleisli f) = Kleisli $ \case
    (Left a) -> Left <$> f a
    (Right b) -> pure $ Right b

class (Applicative f, Traversable f) => Settable f where
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
