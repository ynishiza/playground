{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.List.Extra
import Elevator.Base
import Fmt
import System.IO.Error

main :: IO ()
main = putStrLn "Starting elevator" >> repl (getState initialElevator)

repl :: ElevatorState -> IO ()
repl initialState =
  ( do
      putStrLn "Enter floor or q to exit: "
      input <- trim <$> getLine
      if input == "q"
        then return ()
        else do
          f <- parseFloor input
          (lastState, _) <- simulateFrom initialState [f]
          fmtLn $ "Result:" +|| lastState ||+ ""
          repl lastState
  )
    `Control.Exception.catch` handleErr
  where
    handleErr :: IOException -> IO ()
    handleErr e =
      fmtLn ("error" +| ioeGetErrorString e |+ "")
        >> repl initialState
