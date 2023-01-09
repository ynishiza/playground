{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elevator.Base
  ( MyNum0,
    MyNum1,
    MyNum2,
    MyNum3,
    MyNum4,
    MyNum5,
    MyNum6,
    MyNum7,
    MyNum8,
    MyNum9,
    MyNum10,
    MyNum100,
    f0_10,
    f1_10,
    f2_10,
    f3_10,
    f4_10,
    f5_10,
    f6_10,
    f7_10,
    f8_10,
    f9_10,
    f10_10,
    module X,
    topFloor,
    bottomFloor,
    Mx,
    simulate,
    simulateFromText,
    simulateFrom,
    initialElevator,
    parseFloor,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Catch as X
import Data.Text qualified as T
import Elevator.Elevator as X
import Elevator.Floor as X
import Elevator.FloorTH
import Fmt
import Text.Read

$(genNumAliases 0 100)
$(genFloors 10)

type Mx = MaxFloor_10

mx :: Int
mx = fromIntegral $ snatToNatural $ snat @Mx

topFloor :: Floor Mx MyNum10
topFloor = f10_10

bottomFloor :: Floor Mx MyNum0
bottomFloor = f0_10

initialElevator :: Elevator Mx 'Z 'Opened
initialElevator = MkElevator @Mx @'Z @'Opened

simulateFromText :: T.Text -> IO (ElevatorState, [ElevatorLog])
simulateFromText txt = traverse (parseFloor . T.unpack) inputs >>= simulate
  where
    inputs = filter (not . T.null) $ T.strip <$> T.lines txt

parseFloor :: String -> IO (SomeFloor Mx)
parseFloor input = case readMaybe @Int input of
  Nothing -> throwError $ "Failed to parse " +| input |+ ""
  Just n -> case mkSomeFloor @Mx n of
    Nothing -> throwError $ "Invalid floor number " +| n |+ ". Must be 0 <= x < " +| mx |+ "."
    Just fl -> pure fl

simulate :: [SomeFloor Mx] -> IO (ElevatorState, [ElevatorLog])
simulate = simulateFrom (getState initialElevator)

simulateFrom :: ElevatorState -> [SomeFloor Mx] -> IO (ElevatorState, [ElevatorLog])
simulateFrom st sims = do
  (final, logs, _) <- runElevatorSystem (logElevatorState >> x) st
  return (final, logs)
  where
    x = case mkSomeElevatorFromState st of
      Just e -> foldM_ (flip callSome) e sims
      Nothing -> throwError $ "Failed to create elevator from state " +|| st ||+ ""

throwError :: forall a m. MonadThrow m => String -> m a
throwError m = throwM $ userError m
