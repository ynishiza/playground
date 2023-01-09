{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Elevator.Elevator
  ( Elevator (..),
    SomeElevator (..),
    mkSomeElevatorFromState,
    getFloor,
    getDoorState,
    moveUp,
    moveDown,
    openDoor,
    closeDoor,
    ensureOpenedDoor,
    ensureClosedDoor,
    moveTo,
    call,
    callSome,
    getState,
    module X,
  )
where

import Door.Common as X
import Elevator.Floor
import Elevator.LowLevel as X hiding (closeDoor, moveDown, moveUp, openDoor)
import Elevator.LowLevel qualified as L
import Elevator.Move
import Fmt

type Elevator :: Nat -> Nat -> DoorState -> Type
data Elevator mx f d where
  MkElevator :: (GoodFloor mx f, SDoorStateI d) => Elevator mx f d

data SomeElevator mx where
  MkSomeElevator :: forall mx f d. Elevator mx f d -> SomeElevator mx

instance Show (Elevator mx f d) where
  show e@MkElevator = "Elevator [" +|| getFloor e ||+ "][" +|| getDoorState e ||+ "]"

instance Show (SomeElevator mx) where
  show (MkSomeElevator e@MkElevator) = "SomeElevator (" +|| e ||+ ")"

mkSomeElevatorFromState :: forall mx. SNatI mx => ElevatorState -> Maybe (SomeElevator mx)
mkSomeElevatorFromState (MkElevatorState {..}) = withSomeSing doorState $ mk currentFloor
  where
    mk :: forall d. Int -> SDoorState d -> Maybe (SomeElevator mx)
    mk n s = do
      (MkSomeFloor (MkFloor :: Floor mx a)) <- mkSomeFloor @mx n
      return $ MkSomeElevator $ withSingI s $ MkElevator @mx @a @d

getState :: forall mx f d. Elevator mx f d -> L.ElevatorState
getState MkElevator = L.MkElevatorState (fromIntegral $ snatToNatural $ snat @f) (fromSing $ sing @d)

getFloor :: forall mx f d. GoodFloor mx f => Elevator mx f d -> Floor mx f
getFloor MkElevator = MkFloor @mx @f

getDoorState :: forall mx f d. Elevator mx f d -> DoorState
getDoorState MkElevator = fromSing $ sing @d

moveUp :: forall mx f. (BelowTop mx f) => Elevator mx f 'Closed -> ElevatorSystem (Elevator mx ('S f) 'Closed)
moveUp MkElevator = L.moveUp >> pure MkElevator

moveDown :: Elevator mx ('S f) 'Closed -> ElevatorSystem (Elevator mx f 'Closed)
moveDown e@MkElevator =
  L.moveDown
    >> pure (withFloor (prevFloor $ getFloor e) MkElevator)

openDoor :: Elevator mx f 'Closed -> ElevatorSystem (Elevator mx f 'Opened)
openDoor MkElevator =
  L.openDoor
    >> pure MkElevator

closeDoor :: Elevator mx f 'Opened -> ElevatorSystem (Elevator mx f 'Closed)
closeDoor MkElevator =
  L.closeDoor
    >> pure MkElevator

ensureClosedDoor :: forall mx f d. Elevator mx f d -> ElevatorSystem (Elevator mx f 'Closed)
ensureClosedDoor e@MkElevator =
  withSing @d
    ( \case
        SOpened -> closeDoor e
        SClosed -> pure e
    )

ensureOpenedDoor :: forall mx f d. Elevator mx f d -> L.ElevatorSystem (Elevator mx f 'Opened)
ensureOpenedDoor e@MkElevator =
  withSing @d
    ( \case
        SOpened -> pure e
        SClosed -> openDoor e
    )

moveTo :: Floor mx to -> Elevator mx from 'Closed -> ElevatorSystem (Elevator mx to 'Closed)
moveTo f e@MkElevator = case decideMove f (getFloor e) of
  StandStill -> pure e
  GoingUp -> moveUp e >>= moveTo f
  GoingDown -> moveDown e >>= moveTo f

call :: Floor mx to -> Elevator mx from d -> ElevatorSystem (Elevator mx to 'Opened)
call f e@MkElevator = case sameFloor f (getFloor e) of
  Just p -> gcastWith p $ ensureOpenedDoor e
  Nothing -> ensureClosedDoor e >>= moveTo f >>= ensureOpenedDoor

callSome :: SomeFloor mx -> SomeElevator mx -> ElevatorSystem (SomeElevator mx)
callSome (MkSomeFloor f@MkFloor) (MkSomeElevator e@MkElevator) = MkSomeElevator <$> call f e
