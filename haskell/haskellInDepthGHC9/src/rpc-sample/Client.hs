{-# LANGUAGE QuasiQuotes #-}

module Client where

import RPC.Base

[remote|
hello :: RSIO s String
add :: Int -> Int -> RSIO s Int
  |]
