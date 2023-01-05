{-# LANGUAGE LambdaCase #-}

module Elevator.Elevator
  ( Elevator (..),
    getFloor,
    getDoorState,
    moveUp,
    moveDown,
    openDoor,
    closeDoor,
    ensureOpenedDoor,
    ensureClosedDoor,
    module X,
  )
where

import Control.Monad.IO.Class
import Door.Common as X
import Elevator.Floor

type Elevator :: Nat -> Nat -> DoorState -> Type
data Elevator mx f d where
  MkElevator :: (GoodFloor mx f, SingI d) => Elevator mx f d

getFloor :: forall mx f d. GoodFloor mx f => Elevator mx f d -> Floor mx f
getFloor MkElevator = MkFloor @mx @f

getDoorState :: forall mx f d. Elevator mx f d -> DoorState
getDoorState MkElevator = fromSing $ sing @d

moveUp :: forall mx f m. (MonadIO m, BelowTop mx f) => Elevator mx f 'Closed -> m (Elevator mx ('S f) 'Closed)
moveUp MkElevator =
  liftIO (putStrLn "Moving up")
    >> pure MkElevator

moveDown :: MonadIO m => Elevator mx ('S f) 'Closed -> m (Elevator mx f 'Closed)
moveDown d@MkElevator =
  liftIO (putStrLn "Moving down")
    >> pure (withFloor (prevFloor $ getFloor d) MkElevator)

openDoor :: MonadIO m => Elevator mx f 'Closed -> m (Elevator mx f 'Opened)
openDoor MkElevator =
  liftIO (putStrLn "Opening door")
    >> pure MkElevator

closeDoor :: MonadIO m => Elevator mx f 'Opened -> m (Elevator mx f 'Closed)
closeDoor MkElevator =
  liftIO (putStrLn "Closing door")
    >> pure MkElevator

ensureClosedDoor :: forall m mx f d. MonadIO m => Elevator mx f d -> m (Elevator mx f 'Closed)
ensureClosedDoor e@MkElevator =
  withSing @d
    ( \case
        SOpened -> closeDoor e
        SClosed -> pure e
    )

ensureOpenedDoor :: forall m mx f d. MonadIO m => Elevator mx f d -> m (Elevator mx f 'Opened)
ensureOpenedDoor e@MkElevator =
  withSing @d
    ( \case
        SOpened -> pure e
        SClosed -> openDoor e
    )
