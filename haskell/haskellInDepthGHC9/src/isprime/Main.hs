{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main
  ( main,
    module X,
  )
where

import Control.Monad.Reader
import Prime as X
import System.Environment
import System.TimeIt

main :: IO ()
main = getArgs >>= timeIt . print . isPrime1 . read . head
