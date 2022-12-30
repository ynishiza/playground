{-# OPTIONS_GHC -Wno-orphans #-}

module Common
  ( RemoteObj,
    testParams,
    testBootstrap,
  )
where

import Control.Concurrent
import Network.Connection
import RPC.Common
import System.Random

waitOneSecond :: IO ()
waitOneSecond = threadDelay $ 1 * 1000 * 1000

type RemoteObj = RSIO Int

instance RemoteState Int where
  initState = 0

testParams :: IO RPCParams
testParams = do
  port <- getStdRandom (uniformR @StdGen @Int (10000, 20000))
  let connParams = connectionParams defaultRPCParams
  return
    defaultRPCParams
      { connectionParams = connParams {connectionPort = fromIntegral port}
      }

testBootstrap :: MonadIO m => (RPCParams -> IO ()) -> (RPCParams -> m ()) -> m ()
testBootstrap setup run = do
  params <- liftIO testParams
  tid <- liftIO $ forkIO $ setup (params { sourceName = "server" })
  liftIO waitOneSecond
  run (params { sourceName = "client" })
  liftIO waitOneSecond
  liftIO $ killThread tid
