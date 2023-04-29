{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Lens.Scratch
  ( TakingWhile (..),
    TakingResult (..),
    takingWhileWithoutContravariant,
    takingWhileTW,
  )
where

import Control.Arrow ((>>>))
import Data.Functor.Contravariant
import Debug.Trace (trace)
import Lens.Lens
-- import Lens.TraverseMonoid

-- newtype DeferAp a o t = DeferAp (CaptureAp () t o a)
--   deriving (Show)

-- instance Functor (DeferAp a o) where
--   fmap f (DeferAp d) = DeferAp $ CFmap f d

-- instance Applicative (DeferAp a o) where
--   pure a = DeferAp $ CPure a
--   (DeferAp f) <*> (DeferAp x) = DeferAp $ CAp f x

-- deferAp :: Traversable t => t a -> DeferAp (t a) (t a) (t a)
-- deferAp = traverse (DeferAp . CPure)

data TW f a where
  TW :: Bool -> f a -> (Bool -> f a) -> TW f a

instance Functor f => Functor (TW f) where
  fmap f ~(TW b d h) = TW b (trace "TW fmap f <$> d" $ f <$> d) $ \b' -> if b' then trace "TW fmap if" (f <$> h b') else trace "TW fmap else" (f <$> d)

instance Contravariant f => Contravariant (TW f) where
  contramap f ~(TW b t h) = TW b (trace "TW contramap f >$< t" $ f >$< t) $ \b' -> trace "TW contramap" $ if b' then f >$< h b' else f >$< t

instance Applicative f => Applicative (TW f) where
  pure a = TW True (pure a) $ const (pure a)
  (TW b1 d1 h1) <*> ~(TW b2 d2 h2) = TW (b1 && b2) (trace "TW App d1 <*> d2" $ d1 <*> d2) $ \t ->
    if t
      then trace "TW App if" (h1 True <*> h2 b1)
      else trace "TW App else" (d1 <*> d2)

takingWhileTW :: forall f s t a. Applicative f => LensLike (TW f) s t a a -> (a -> Bool) -> LensLike f s t a a
takingWhileTW lens predicate useA = lens (createTakeWhile useA) >>> run
  where
    run :: TW f v -> f v
    run (TW _ _ x) = trace "takingWhileTW $" $ x True
    createTakeWhile :: (a -> f a) -> a -> TW f a
    createTakeWhile get a = TW (predicate a) (pure a) $ \b -> if b && predicate a then get a else pure a

data TakingResult a f b where
  TakeNext :: f a -> TakingResult a f b
  TakeDone :: f b -> TakingResult a f b

instance Functor f => Functor (TakingResult a f) where
  fmap f (TakeDone x) = TakeDone $ f <$> x
  fmap _ (TakeNext x) = TakeNext x

-- NOTE: attempt to define TakingWhile without a Contravariant
data TakingWhile a f b where
  TakingWhile :: {witnessTW :: f b, runTW :: Int -> TakingResult a f b} -> TakingWhile a f b

instance Functor f => Functor (TakingWhile a f) where
  fmap f (TakingWhile w run) = TakingWhile (f <$> w) $ (f <$>) . run

instance Contravariant f => Contravariant (TakingWhile a f) where
  contramap f (TakingWhile w run) = TakingWhile (contramap f w) $ \i -> trace "C" $ case run i of
    (TakeDone d) -> trace "CD" $ TakeDone $ contramap f d
    (TakeNext x) -> trace "CN" $ TakeNext x

instance Applicative f => Applicative (TakingWhile a f) where
  pure a = TakingWhile (pure a) (\_ -> TakeDone $ pure a)
  t1 <*> t2 = TakingWhile (witnessTW t1 <*> witnessTW t2) $ \i -> trace "T" $ case runTW t1 i of
    (TakeDone d1) -> trace "D1" $ TakeDone (d1 *> witnessTW t1 <*> witnessTW t2)
    (TakeNext x) -> trace "N1" $ case runTW t2 (i + 1) of
      (TakeDone d2) -> trace "D2" $ TakeDone $ x *> (witnessTW t1 <*> d2)
      (TakeNext x2) -> trace "N2" $ TakeNext $ x *> x2

takingWhileWithoutContravariant :: forall f s t a. Applicative f => LensLike (TakingWhile a f) s t a a -> (Int -> a -> Bool) -> LensLike f s t a a
takingWhileWithoutContravariant lens predicate useA =
  lens (createTakeWhile useA)
    >>> run
  where
    run :: TakingWhile u f v -> f v
    run (TakingWhile w f) = case f 0 of
      (TakeDone d) -> trace "r1" d
      (TakeNext x) -> trace "r2" $ x *> w
    createTakeWhile :: (a -> f a) -> a -> TakingWhile a f a
    createTakeWhile get a = TakingWhile (pure a) $ \i -> if predicate i a then TakeNext (get a) else TakeDone (pure a)
