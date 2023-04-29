{-# LANGUAGE GADTs #-}

module Lens.Monoid.FreeTake
  ( FreeTake (..),
    runFreeTake,
    FreeDrop (..),
    ruunFreeDrop,
  )
where

import Control.Arrow (Arrow (..))
import Data.Functor.Contravariant
import Lens.Monoid.FreeAp

data FreeTake i a o x where
  FreeTake :: x -> ((i, Bool) -> (FreeAp a o x, Bool)) -> FreeTake i a o x

runFreeTake :: Enum i => FreeTake i a o x -> FreeAp a o x
runFreeTake (FreeTake _ ca) = fst (ca (toEnum 0, True))

instance Functor (FreeTake i a o) where
  fmap g (FreeTake x taker) = FreeTake (g x) $ first (FFmap g) . taker

instance Enum i => Applicative (FreeTake i a o) where
  pure x = FreeTake x $ \(_, isTaking) -> (FPure x, isTaking)
  (FreeTake f takerf) <*> ~(FreeTake x takerx) = FreeTake (f x) $ \(i, isTaking) ->
    let (freef, isTaking') = takerf (i, isTaking)
     in if isTaking
          then first (FAp freef) $ takerx (succ i, isTaking')
          else (FPure (f x), False)

data FreeDrop i a o x where
  FreeDrop :: x -> ((i, Bool) -> (FreeAp a o x, Bool)) -> FreeDrop i a o x

ruunFreeDrop :: Enum i => FreeDrop i a o x -> FreeAp a o x
ruunFreeDrop (FreeDrop _ dp) = fst (dp (toEnum 0, True))

instance Functor (FreeDrop i a o) where
  fmap f (FreeDrop x dropper) = FreeDrop (f x) $ first (FFmap f) . dropper

instance Enum i => Applicative (FreeDrop i a o) where
  pure x = FreeDrop x $ \(_, isDropping) -> (FPure x, isDropping)
  (FreeDrop f dropperf) <*> ~(FreeDrop x dropperx) = FreeDrop (f x) $ \(i, isDropping) ->
    let (freef, isDropping') = dropperf (i, isDropping)
     in case (isDropping, isDropping') of
          -- case: drop first term
          (True, True) -> first (FAp (FPure f)) $ dropperx (succ i, True)
          -- case: no longer dropping
          _ -> (FAp freef $ fst $ dropperx (succ i, False), False)

instance Contravariant (FreeTake i a o) where
  contramap _ = (<$) $ error "Only to use as phantom"

instance Contravariant (FreeDrop i a o) where
  contramap _ = (<$) $ error "Only to use as phantom"
