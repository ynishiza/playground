{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Server
  ( 
  )
where

import RPC.Base

declareRPCTable "t2" ['hello]

hello :: String -> RSIO () String
hello = undefined
