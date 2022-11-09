{-# LANGUAGE OverloadedStrings #-}

module Utils (
  promptRun,
  trace,
  traceShow,
  ) where

import Debug.Trace(trace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt

traceShow :: Show a1 => a1 -> a2 -> a2
traceShow x = trace (show x) 

promptRun :: T.Text -> IO () -> IO Bool
promptRun message action = do
  TIO.putStrLn $ fmt "Run "+|message|+"?"
  response <- getChar
  if T.pack [response] == "y"
    then do
      TIO.putStrLn $ fmt "Running "+|message|+""
      action
      return True
    else do
      TIO.putStrLn $ fmt "Skipping "+|message|+""
      return False
