{-# LANGUAGE OverloadedStrings #-} 

module Main (main) where

import qualified Lib1
import qualified B.Lib
import Fmt

main :: IO ()
main = do 
  fmtLn "App1 test"
  Lib1.run
  B.Lib.run
