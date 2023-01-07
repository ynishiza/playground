{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
    initialElevator,
    test,
  )
where

import Control.Monad
import Control.Exception
import Text.Read
import Data.Text qualified as T
import Elevator.Elevator as X
import Elevator.Floor as X
import Elevator.FloorTH
import Fmt

$(genNumAliases 0 100)
$(genFloors 10)

type Mx = MaxFloor_10
mx :: Int
mx = fromIntegral $ snatToNatural $ snat @Mx

topFloor :: Floor Mx MyNum10
topFloor = f10_10

bottomFloor :: Floor Mx MyNum0
bottomFloor = f0_10

initialElevator :: Elevator Mx 'Z 'Closed
initialElevator = MkElevator @Mx @'Z @'Closed

simulateFromText :: T.Text -> IO (ElevatorState, [ElevatorLog])
simulateFromText txt = simulate $ prep . T.unpack <$> l
  where
    l = filter (not . T.null) $ T.strip <$> T.lines txt
    prep term = case readMaybe @Int term of
        Nothing -> throw $ userError $ "Failed to parse "+|term|+""
        Just n -> case mkSomeFloor @Mx n of
                  Nothing -> throw $ userError $ "Invalid floor number " +|n|+". Must be 0 <= x < " +|mx|+"."
                  Just fl -> fl

simulate :: [SomeFloor Mx] -> IO (ElevatorState, [ElevatorLog])
simulate sims = do
  (final, logs, _) <- runElevatorSystem (logElevatorState >> x) (getState initialElevator)
  return (final, logs)
  where
    x = foldM_ (flip callSome) (MkSomeElevator initialElevator) sims

test = simulate [MkSomeFloor (MkFloor @Mx @Nat5)]
