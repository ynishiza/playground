{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- ORMOLU_DISABLE -}
module RadarSafe
  ( 
    SDirection (..),
    Direction (..),
    SafeTurn(..),

    SafeRadar (..),
    SomeSafeRadar,
    mkSafeRadar,
    mkSomeSafeRadar,

    Clockwise,
    AntiClockwise,
    getDirection,
    toClockWise,
    toAntiClockwise,
    rotateClockwise,
    rotateAnticlockwise,
    rotateAround,
    rotateClockwiseIO,
    rotateAntiClockwiseIO,
    rotateAroundIO,
    rotateToIO,
    rotateUnit,

    module X,
    rn,
    re,
    rs,
    rw,
    allRadars,
    NorthSym0,
    EastSym0,
    WestSym0,
    SouthSym0,

    -- old
    canRotateClockwiseTo,
    canRotateAntiClockwiseTo,
    canRotateOne,
  )
where
{- ORMOLU_ENABLE -}

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Kind
import Data.Ord.Singletons
import Data.Singletons.Base.Enum
import Data.Singletons.Sigma as X
import Data.Singletons.TH as X
import Data.String.Singletons
import Data.Type.Dec
import Data.Typeable
import Fmt
import Prelude.Singletons

$( singletons
     [d|
       data Direction where
         North :: Direction
         East :: Direction
         South :: Direction
         West :: Direction
         deriving (Show, Eq, Ord, Bounded)

       data A where
         A :: A
         B :: A
         deriving (Show, Eq, Ord, Bounded)
       |]
 )

type SafeRadar :: Direction -> Type
data SafeRadar d where
  MkSafeRadar :: SingI d => SafeRadar d

-- note: like the original Turn, but annotated by Direction
-- In particular, needed for rotation proofs 
-- e.g. for proving/disproving (Clockwise d :~: d')
type SafeTurn :: Direction -> Direction -> Type
data SafeTurn d e where
  TurnClockwise :: Clockwise d ~ e => SafeTurn d e
  TurnAntiClockwise :: AntiClockwise d ~ e => SafeTurn d e
  TurnAround :: Clockwise (Clockwise d) ~ e => SafeTurn d e
  TurnNone :: SafeTurn d d

deriving instance Typeable (SafeRadar d)

deriving instance Typeable Direction

instance Eq (SafeRadar d) where
  _ == _ = True

instance Show (SafeRadar d) where
  show MkSafeRadar = fmt $ "Radar " +|| getDirection (sing @d) ||+ ""

getDirection :: SDirection d -> Direction
getDirection = fromSing

instance Eq (SDirection d) where
  _ == _ = True

type SomeSafeRadar = Sigma Direction (TyCon1 SafeRadar)

mkSafeRadar :: Direction -> SomeSafeRadar
mkSafeRadar d = f $ toSing d
  where
    f :: SomeSing Direction -> SomeSafeRadar
    f (SomeSing d') = withSingI d' $ d' :&: MkSafeRadar

mkSomeSafeRadar :: forall d. SafeRadar d -> SomeSafeRadar
mkSomeSafeRadar MkSafeRadar = sing @d :&: MkSafeRadar

instance PEnum Direction where
  type FromEnum 'North = 0
  type FromEnum 'East = 1
  type FromEnum 'South = 2
  type FromEnum 'West = 3
  type ToEnum 0 = 'North
  type ToEnum 1 = 'East
  type ToEnum 2 = 'South
  type ToEnum 3 = 'West

type family Clockwise d where
  Clockwise 'West = 'North
  Clockwise d = Succ d

type family AntiClockwise d where
  AntiClockwise 'North = 'West
  AntiClockwise a = Pred a

type Around d = Clockwise (Clockwise d)

toClockWise :: SDirection (d :: Direction) -> SDirection (Clockwise d)
toClockWise SNorth = SEast
toClockWise SEast = SSouth
toClockWise SSouth = SWest
toClockWise SWest = SNorth

toAntiClockwise :: SDirection (d :: Direction) -> SDirection (AntiClockwise d)
toAntiClockwise SNorth = SWest
toAntiClockwise SEast = SNorth
toAntiClockwise SSouth = SEast
toAntiClockwise SWest = SSouth

getSafeTurn :: forall d e. SDirection d -> SDirection e -> SafeTurn d e
getSafeTurn SNorth SNorth = TurnNone
getSafeTurn SNorth SEast = TurnClockwise
getSafeTurn SNorth SSouth = TurnAround
getSafeTurn SNorth SWest = TurnAntiClockwise
getSafeTurn SEast SEast = TurnNone
getSafeTurn SEast SSouth = TurnClockwise
getSafeTurn SEast SWest = TurnAround
getSafeTurn SEast SNorth = TurnAntiClockwise
getSafeTurn SSouth SSouth = TurnNone
getSafeTurn SSouth SWest = TurnClockwise
getSafeTurn SSouth SNorth = TurnAround
getSafeTurn SSouth SEast = TurnAntiClockwise
getSafeTurn SWest SWest = TurnNone
getSafeTurn SWest SNorth = TurnClockwise
getSafeTurn SWest SEast = TurnAround
getSafeTurn SWest SSouth = TurnAntiClockwise

rotateClockwise :: forall d e. (e ~ Clockwise d) => SafeRadar d -> SafeRadar e
rotateClockwise MkSafeRadar = withSingI (toClockWise (sing @d)) MkSafeRadar

rotateAnticlockwise :: forall d e. (e ~ AntiClockwise d) => SafeRadar d -> SafeRadar e
rotateAnticlockwise MkSafeRadar = withSingI (toAntiClockwise (sing @d)) MkSafeRadar

rotateAround :: forall d e. (e ~ Around d) => SafeRadar d -> SafeRadar e
rotateAround = rotateClockwise . rotateClockwise

rotateToIO :: forall m d e. MonadIO m => SafeRadar d -> SDirection e -> m (SafeRadar e)
rotateToIO r@MkSafeRadar e = case getSafeTurn (sing @d) e of
  TurnNone -> pure r
  TurnClockwise -> rotateClockwiseIO r
  TurnAntiClockwise -> rotateAntiClockwiseIO r
  TurnAround -> rotateAroundIO r

rotateClockwiseIO :: forall m d e. (e ~ Clockwise d, MonadIO m) => SafeRadar d -> m (SafeRadar e)
rotateClockwiseIO d@MkSafeRadar = liftIO (putStrLn (rotateLog d to)) >> pure to
  where
    to = rotateClockwise d

rotateAntiClockwiseIO :: forall m d e. (e ~ AntiClockwise d, MonadIO m) => SafeRadar d -> m (SafeRadar e)
rotateAntiClockwiseIO d@MkSafeRadar = liftIO (putStrLn (rotateLog d to)) >> pure to
  where
    to = rotateAnticlockwise d

rotateAroundIO :: forall m d e. (e ~ Around d, MonadIO m) => SafeRadar d -> m (SafeRadar e)
rotateAroundIO d@MkSafeRadar = liftIO (putStrLn (rotateLog d to)) >> pure to
  where
    to = rotateAround d

rotateLog :: forall d e. SafeRadar d -> SafeRadar e -> String
rotateLog MkSafeRadar MkSafeRadar = fmt $ d1 ||+ "\t->\t" +|| d2 ||+ ""
  where
    d1 = sing @d
    d2 = withSingI d1 $ sing @e

rotateUnit :: forall (d :: Direction) (e :: Direction) m. (MonadIO m, MonadThrow m) => SafeRadar d -> SDirection e -> m (SafeRadar e)
rotateUnit r@MkSafeRadar e = case getSafeTurn (sing @d) e of
  TurnClockwise -> rotateClockwiseIO r
  TurnAntiClockwise -> rotateAntiClockwiseIO r
  _ -> throwM $ userError $ "Can only turn clockwise to " +|| getDirection c ||+ " or anti-clockwise to " +|| getDirection ac ||+ ""
    where
      c = toClockWise (sing @d)
      ac = toAntiClockwise (sing @d)

rn :: SafeRadar 'North
rn = MkSafeRadar @'North

re :: SafeRadar 'East
re = MkSafeRadar @'East

rs :: SafeRadar 'South
rs = MkSafeRadar @'South

rw :: SafeRadar 'West
rw = MkSafeRadar @'West

allRadars :: [SomeSafeRadar]
allRadars =
  [ SNorth :&: rn,
    SEast :&: re,
    SSouth :&: rs,
    SWest :&: rw
  ]

-- ================================================== OLD ============================================================
--

type CanRotateClockwiseTo from to = Clockwise from :~: to

type CanRotateAntiClockwiseTo from to = AntiClockwise from :~: to

type CanRotateOne from to = Either (CanRotateClockwiseTo from to) (CanRotateAntiClockwiseTo from to)

canRotateClockwiseTo :: forall d (e :: Direction). SafeRadar d -> SDirection e -> Dec (CanRotateClockwiseTo d e)
canRotateClockwiseTo MkSafeRadar e = case (sing @d, e) of
  (SNorth, SEast) -> Yes Refl
  (SEast, SSouth) -> Yes Refl
  (SSouth, SWest) -> Yes Refl
  (SWest, SNorth) -> Yes Refl
  _ -> No $ \case {}

canRotateAntiClockwiseTo :: forall d (e :: Direction). SafeRadar d -> SDirection e -> Dec (CanRotateAntiClockwiseTo d e)
canRotateAntiClockwiseTo MkSafeRadar e = case (sing @d, e) of
  (SNorth, SWest) -> Yes Refl
  (SEast, SNorth) -> Yes Refl
  (SSouth, SEast) -> Yes Refl
  (SWest, SSouth) -> Yes Refl
  _ -> No $ \case {}

-- NOTE: is it possible to prove this with SafeTurn?
-- canRotateAntiClockwiseTo MkSafeRadar e = case getSafeTurn (sing @d) e of
--                                            TurnAntiClockwise -> Yes Refl
--                                            _ -> No $ \case 
--                                             x -> ?

canRotateOne :: forall d (e :: Direction). SafeRadar d -> SDirection e -> Dec (CanRotateOne d e)
canRotateOne d e = case canRotateClockwiseTo d e of
  Yes Refl -> Yes $ Left Refl
  No rv1 -> case canRotateAntiClockwiseTo d e of
    Yes Refl -> Yes $ Right Refl
    No rv2 -> No $ \case
      (Left v) -> rv1 v
      (Right v) -> rv2 v
