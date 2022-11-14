{-# LANGUAGE OverloadedStrings #-}

module Utils (
  promptRun,
  trace,
  traceShow,
  traceShowId,
  assertIsEqual,
  assertIsEqualSilent,
  printBanner,
  ) where

import Control.Monad
import Debug.Trace(trace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt

traceShow :: Show a1 => a1 -> a2 -> a2
traceShow x = trace (show x) 

traceShowId :: Show a1 => String -> a1 -> a1
traceShowId label x = trace (label|+" "+||x||+"") x

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

assertIsEqual :: (Eq a, Show a) => a -> a -> IO ()
assertIsEqual x y = do
  assertIsEqualBase True x y 

assertIsEqualSilent :: (Eq a, Show a) => a -> a -> IO ()
assertIsEqualSilent x y = do
  assertIsEqualBase False x y 

assertIsEqualBase :: (Eq a, Show a) => Bool -> a -> a -> IO ()
assertIsEqualBase showMessage x y = do
  unless (x == y) $ error $ fmtLn $ "assertIsEqual:"+||x||+"!="+||y||+""
  when showMessage $ fmtLn $ "assertIsEqual:" +|| x ||+ "==" +|| y ||+""

printBanner :: T.Text -> IO ()
printBanner name = fmtLn $ "====="+| name |+ "====="
