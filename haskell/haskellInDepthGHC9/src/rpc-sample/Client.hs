{-# LANGUAGE QuasiQuotes #-}

module Client where

import RPC.Base

[remote|
hello :: RSIO () String
add :: Int -> Int -> RSIO () Int
  |]
