{-# LANGUAGE OverloadedStrings #-} 

module Main (main) where

import System.Directory
import qualified Lib1
import qualified B.Lib
import Fmt

main :: IO ()
main = do 
  fmtLn "App1"
  Lib1.run
  B.Lib.run
  getCurrentDirectory >>= (fmtLn . ("CWD="+|) . build)
