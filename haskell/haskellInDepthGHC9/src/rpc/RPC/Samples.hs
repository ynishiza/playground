{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module RPC.Samples
  ( sampleTable,
  serverProcess,
  clientProcess,
  add,
  hello,
  divide,
  RPC.Samples.sum,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Serialize
import RPC.Common
import RPC.Messaging
import RPC.Parse
import RPC.Client

[remote|
hello :: RSIO () String
add :: Int -> Int -> RSIO () Int
divide :: Double -> Double -> RSIO () Double
sum :: [Int] -> RSIO () Int
  |]

sampleTable :: RPCTable s
sampleTable =
  [ ( "hello",
      const $ pure $ encode @String "World"
    ),
    ( "add",
      \d -> do
        (x, y) <- forceDecodeParam @(Int, Int) Stage2 d
        pure $ encode $ x + y
    ),
    ( "sum",
      \d -> do
        list <- forceDecodeParam @[Int] Stage2 d
        pure $ encode $ Prelude.sum list
    ),
    ( "divide",
      \d -> do
        (x, y) <- forceDecodeParam @(Double, Double) Stage2 d
        when (y == 0) (throwM DivideByZero)
        pure $ encode $ x / y
    )
  ]

serverProcess :: RSIO () ()
serverProcess = do
  liftIO $ putStrLn "waiting"
  d <- receiveData >>= forceDecodeParam @String Stage2
  logDebugN $ "received:" +|| d ||+ ""
  logDebugN "send"
  liftIO $ threadDelay $ 5 * 1000 * 1000
  sendData $ encode @String "World"
  logDebugN "done"

clientProcess :: RSIO () ()
clientProcess = do
  logDebugN "send"
  sendData $ encode @String "Hello"
  logDebugN "waiting"
  d <- decode @String <$> receiveData
  logDebugN $ "received:" +|| d ||+ ""
