{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Lens.TraverseMonoid
  ( CaptureAp (..),
    TakingWhileCaptured (..),
    runTakingWhileCaptured,
    runCapturedAp,
    DroppingWhileApplicative (..),
    DroppingWhileCaptured (..),
    runDroppingWhileCaptured,
  )
where

import Control.Arrow (Arrow (..))
import Data.Functor.Contravariant
import Data.Typeable

data CaptureAp i a o captured where
  CPure :: a -> CaptureAp i a o captured
  CAp :: CaptureAp i (x -> y) o captured -> CaptureAp i x o captured -> CaptureAp i y o captured
  CFmap :: (x -> y) -> CaptureAp i x o captured -> CaptureAp i y o captured
  CCapture :: i -> captured -> CaptureAp i o o captured
  deriving stock (Typeable)

instance Foldable (CaptureAp i a o) where
  foldr _ r (CPure _) = r
  foldr f r (CAp df da) = foldr f (foldr f r df) da
  foldr f r (CFmap _ da) = foldr f r da
  foldr f r (CCapture _ captured) = f captured r

instance Traversable (CaptureAp i a o) where
  traverse _ (CPure a) = pure $ CPure a
  traverse f (CAp df da) = CAp <$> traverse f df <*> traverse f da
  traverse f (CFmap g da) = CFmap g <$> traverse f da
  traverse f (CCapture i captured) = CCapture i <$> f captured

instance Functor (CaptureAp i a o) where
  fmap _ (CPure a) = CPure a
  fmap f (CAp df da) = CAp (f <$> df) (f <$> da)
  fmap f (CFmap g da) = CFmap g (f <$> da)
  fmap f (CCapture i captured) = CCapture i $ f captured

instance Show captured => Show (CaptureAp i a o captured) where
  show (CPure _) = "CPure"
  show (CAp f a) = "(CAp " <> show f <> " " <> show a <> ")"
  show (CFmap _ a) = "(CFmap f " <> show a <> ")"
  show (CCapture _ v) = "(CResult " <> show v <> ")"

runCapturedAp :: CaptureAp i a captured captured -> a
runCapturedAp (CPure t) = t
runCapturedAp (CAp df da) = runCapturedAp df $ runCapturedAp da
runCapturedAp (CFmap f da) = f $ runCapturedAp da
runCapturedAp (CCapture _ captured) = captured

data TakingWhileCaptured i captured o a where
  TakingWhileCaptured :: a -> ((i, Bool) -> (CaptureAp () a o captured, Bool)) -> TakingWhileCaptured i captured o a

runTakingWhileCaptured :: Enum i => TakingWhileCaptured i captured o a -> CaptureAp () a o captured
runTakingWhileCaptured (TakingWhileCaptured _ f) = fst $ f (toEnum 0, True)

instance Functor (TakingWhileCaptured i captured o) where
  fmap f (TakingWhileCaptured witness ca) = TakingWhileCaptured (f witness) $ \(i, isTaking) ->
    if isTaking
      then
        let (x, isTakingX) = ca (i, isTaking)
         in (CFmap f x, isTakingX)
      else (CPure (f witness), False)

instance Enum i => Applicative (TakingWhileCaptured i captured o) where
  pure a = TakingWhileCaptured a $ \(_, isTaking) -> (CPure a, isTaking)
  (TakingWhileCaptured witnessf cf) <*> ~(TakingWhileCaptured witnessa ca) = TakingWhileCaptured (witnessf witnessa) $ \(i, isTaking) ->
    if isTaking
      then
        let (f, isTakingF) = cf (i, isTaking)
         in first (CAp f) (ca (succ i, isTakingF))
      else (CPure (witnessf witnessa), False)

instance Contravariant (TakingWhileCaptured i a o) where
  contramap _ = (<$) $ error "Contravariant"

data DroppingWhileCaptured i captured o a where
  DroppingWhileCaptured :: a -> ((i, Bool) -> (CaptureAp () a o captured, Bool)) -> DroppingWhileCaptured i captured o a

runDroppingWhileCaptured :: Enum i => DroppingWhileCaptured i captured o a -> CaptureAp () a o captured
runDroppingWhileCaptured (DroppingWhileCaptured _ d) = fst $ d (toEnum 0, True)

instance Functor (DroppingWhileCaptured i captured o) where
  fmap f (DroppingWhileCaptured witness da) = DroppingWhileCaptured (f witness) $ \v -> first (CFmap f) (da v)

instance Enum i => Applicative (DroppingWhileCaptured i captured o) where
  pure a = DroppingWhileCaptured a $ \(_, isDropping) -> (CPure a, isDropping)
  (DroppingWhileCaptured witnessf df) <*> ~(DroppingWhileCaptured witnessa da) = DroppingWhileCaptured (witnessf witnessa) $ \(i, isDropping) ->
    if not isDropping
      then (CAp (fst $ df (i, False)) (fst $ da (succ i, False)), False)
      else case df (i, True) of
        (f, False) -> first (CAp f) $ da (succ i, False)
        (_, True) -> first (CAp (CPure witnessf)) $ da (succ i, True)

instance Contravariant (DroppingWhileCaptured i captured o) where
  contramap _ = (<$) $ error "Contravariant"

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

capturedToList :: CaptureAp i a o captured -> [captured]
capturedToList = foldr (:) []

mapCaptured :: ([captured], CaptureAp i a captured captured) -> ([captured], a)
mapCaptured ([], c) = ([], runCapturedAp c)
mapCaptured (as, CPure v) = (as, v)
mapCaptured (a:as, CCapture _ _) = (as, a)
mapCaptured (as, CFmap f x) = (as', f x')
  where
    (as', x') = mapCaptured (as, x)
mapCaptured (as, CAp f x) = (as'', f' x')
  where
    (as', f') = mapCaptured (as, f)
    (as'', x') = mapCaptured (as', x)

