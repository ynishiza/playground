{-# LANGUAGE OverloadedStrings #-} 

module Main (main) where

import qualified Lib2
import qualified B.Lib
import Fmt

main :: IO ()
main = do 
  fmtLn "App2 test"
  Lib2.run
  B.Lib.run
