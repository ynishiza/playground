{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Lens.TraverseMonoid
  ( CaptureAp (..),
    TakingWhileCaptured (..),
    runTakingWhileCaptured,
    runCapturedAp,
    DroppingWhileApplicative (..),
  )
where

import Control.Arrow (Arrow (..))
import Data.Functor.Contravariant
import Data.Typeable

data CaptureAp i a o result where
  CPure :: a -> CaptureAp i a o result
  CAp :: CaptureAp i (x -> y) o result -> CaptureAp i x o result -> CaptureAp i y o result
  CFmap :: (x -> y) -> CaptureAp i x o result -> CaptureAp i y o result
  CResult :: i -> result -> CaptureAp i o o result
  deriving stock (Typeable)

instance Foldable (CaptureAp i a o) where
  foldr _ r (CPure _) = r
  foldr f r (CAp df da) = foldr f (foldr f r df) da
  foldr f r (CFmap _ da) = foldr f r da
  foldr f r (CResult _ result) = f result r

instance Traversable (CaptureAp i a o) where
  traverse _ (CPure a) = pure $ CPure a
  traverse f (CAp df da) = CAp <$> traverse f df <*> traverse f da
  traverse f (CFmap g da) = CFmap g <$> traverse f da
  traverse f (CResult i result) = CResult i <$> f result

instance Functor (CaptureAp i a o) where
  fmap _ (CPure a) = CPure a
  fmap f (CAp df da) = CAp (f <$> df) (f <$> da)
  fmap f (CFmap g da) = CFmap g (f <$> da)
  fmap f (CResult i result) = CResult i $ f result

instance Show result => Show (CaptureAp i a o result) where
  show (CPure _) = "CPure"
  show (CAp f a) = "(CAp " <> show f <> " " <> show a <> ")"
  show (CFmap _ a) = "(CFmap f " <> show a <> ")"
  show (CResult _ v) = "(CResult " <> show v <> ")"

runCapturedAp :: CaptureAp i a result result -> a
runCapturedAp (CPure t) = t
runCapturedAp (CAp df da) = runCapturedAp df $ runCapturedAp da
runCapturedAp (CFmap f da) = f $ runCapturedAp da
runCapturedAp (CResult _ result) = result

data TakingWhileCaptured i result o a where
  TakingWhileCaptured :: a -> ((i, Bool) -> (CaptureAp () a o result, Bool)) -> TakingWhileCaptured i result o a

runTakingWhileCaptured :: Enum i => TakingWhileCaptured i result o a -> CaptureAp () a o result
runTakingWhileCaptured (TakingWhileCaptured _ f) = fst $ f (toEnum 0, True)

instance Functor (TakingWhileCaptured i result o) where
  fmap f (TakingWhileCaptured witness ca) = TakingWhileCaptured (f witness) $ \(i, isTaking) ->
    if isTaking
      then
        let (x, isTakingX) = ca (i, isTaking)
         in (CFmap f x, isTakingX)
      else (CPure (f witness), False)

instance Enum i => Applicative (TakingWhileCaptured i result o) where
  pure a = TakingWhileCaptured a $ \(_, isTaking) -> (CPure a, isTaking)
  (TakingWhileCaptured witnessf cf) <*> ~(TakingWhileCaptured witnessa ca) = TakingWhileCaptured (witnessf witnessa) $ \(i, isTaking) ->
    if isTaking
      then
        let (f, isTakingF) = cf (i, isTaking)
            (x, isTakingFx) = ca (succ i, isTakingF)
         in (CAp f x, isTakingFx)
      else (CPure (witnessf witnessa), False)

instance Contravariant (TakingWhileCaptured i a o) where
  contramap _ = (<$) $ error ""

data DroppingWhileApplicative f a where
  DroppingWhileApplicative :: {runDroppingWhileApplicative :: (Int, Bool) -> (f a, Bool)} -> DroppingWhileApplicative f a

instance Functor f => Functor (DroppingWhileApplicative f) where
  fmap f (DroppingWhileApplicative dropA) = DroppingWhileApplicative $ first (f <$>) . dropA

instance Contravariant f => Contravariant (DroppingWhileApplicative f) where
  contramap f (DroppingWhileApplicative dropA) = DroppingWhileApplicative $ first (contramap f) . dropA

instance Applicative f => Applicative (DroppingWhileApplicative f) where
  pure a = DroppingWhileApplicative $ \(_, dropping) -> (pure a, dropping)
  dropF <*> dropA = DroppingWhileApplicative $ \(i, dropping) ->
    let (f, d1) = runDroppingWhileApplicative dropF (i, dropping)
     in first (f <*>) $ runDroppingWhileApplicative dropA (i + 1, d1)

data DroppingWhileR a where
  DroppingWhileR :: {runDroppingWhileR :: (Int, Bool) -> (a, Bool)} -> DroppingWhileR a

instance Semigroup a => Semigroup (DroppingWhileR a) where
  d1 <> d2 = DroppingWhileR $ \(i, isDropping) ->
    let (x1, dropX1) = runDroppingWhileR d1 (i, isDropping)
     in case (isDropping, dropX1) of
          -- case: drop x1
          (True, True) -> runDroppingWhileR d2 (i + 1, True)
          -- case: take x1
          _ -> first (x1 <>) $ runDroppingWhileR d2 (i + 1, False)

instance Monoid a => Monoid (DroppingWhileR a) where
  mempty = DroppingWhileR $ const (mempty, True)
