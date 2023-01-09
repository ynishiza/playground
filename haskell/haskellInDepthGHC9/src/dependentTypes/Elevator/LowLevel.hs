{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elevator.LowLevel
  ( moveUp,
    moveDown,
    openDoor,
    closeDoor,
    ElevatorSystem (..),
    ElevatorLog (..),
    ElevatorState (..),
    runElevatorSystem,
    logElevatorState,
    logElevatorStateWith,
  )
where

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Writer
import Door.Common

newtype ElevatorSystem a = Runner (WriterT [ElevatorLog] (StateT ElevatorState IO) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadWriter [ElevatorLog],
      MonadState ElevatorState
    )

data ElevatorLog where
  MkElevatorLog ::
    { message :: !(Maybe String),
      state :: !ElevatorState
    } ->
    ElevatorLog
  deriving (Eq, Show)

data ElevatorState where
  MkElevatorState ::
    { currentFloor :: !Int,
      doorState :: !DoorState
    } ->
    ElevatorState
  deriving (Eq, Show)

runElevatorSystem :: ElevatorSystem a -> ElevatorState -> IO (ElevatorState, [ElevatorLog], a)
runElevatorSystem (Runner s) st = do
  ((v, logs), final) <- runStateT (runWriterT s) st
  pure (final, logs, v)

moveUp :: ElevatorSystem ()
moveUp =
  updateElevatorState (\e -> e {currentFloor = currentFloor e + 1})
    >> liftIO (putStrLn "Moving up")

moveDown :: ElevatorSystem ()
moveDown =
  updateElevatorState (\e -> e {currentFloor = currentFloor e - 1})
    >> liftIO (putStrLn "Moving down")

openDoor :: ElevatorSystem ()
openDoor =
  updateElevatorState (\e -> e {doorState = Opened})
    >> liftIO (putStrLn "Opening door")

closeDoor :: ElevatorSystem ()
closeDoor =
  updateElevatorState (\e -> e {doorState = Closed})
    >> liftIO (putStrLn "Closing door")

updateElevatorState :: (ElevatorState -> ElevatorState) -> ElevatorSystem ()
updateElevatorState f = get >>= updateElevatorStateWith . f

updateElevatorStateWith :: ElevatorState -> ElevatorSystem ()
updateElevatorStateWith s = put s >> logElevatorStateWith s

logElevatorState :: ElevatorSystem ()
logElevatorState = get >>= logElevatorStateWith

logElevatorStateWith :: ElevatorState -> ElevatorSystem ()
logElevatorStateWith st = tell [MkElevatorLog Nothing st]
