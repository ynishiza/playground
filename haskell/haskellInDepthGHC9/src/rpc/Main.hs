{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Control.Concurrent
import Data.Serialize
import Network.Connection
import Network.Socket (PortNumber)
import Options.Applicative
import RPC.Client qualified as Client
import RPC.Messaging
import RPC.Server qualified as Server

main :: IO ()
main = do
  argParams@(MkParams {..}) <- execParser parserInfo
  fmtLn $ nameF "CLI rsParams" (build argParams)
  let rsParams =
        MkRPCParams
          { logLevel = if debug then LevelDebug else LevelInfo,
            connectionParams = ConnectionParams host port Nothing Nothing,
            connection = undefined
          }
  case () of
    ()
      | server -> Server.startOperationServer rsParams (Server.sampleTable @())
      | hello -> Client.requestOperation rsParams "hello" () >>= print @String
      | (Just p) <- add -> Client.requestOperation rsParams "add" p >>= print @Int
      | (Just p) <- divide -> Client.requestOperation rsParams "divide" p >>= print @Double
      | otherwise -> Client.requestOperation rsParams "BADOP" ()

data Params = MkParams
  { server :: !Bool,
    host :: !String,
    port :: !PortNumber,
    debug :: !Bool,
    hello :: !Bool,
    add :: !(Maybe (Int, Int)),
    divide :: !(Maybe (Double, Double))
  }
  deriving (Show, Eq)

instance Buildable Params where build = build . show

parserInfo :: ParserInfo Params
parserInfo = info (helper <*> parser) (fullDesc <> progDesc "")

parser :: Parser Params
parser =
  MkParams
    <$> flag False True (long "server")
    <*> option auto (long "host" <> value "0.0.0.0" <> metavar "IP")
    <*> option auto (long "port" <> value 12345 <> metavar "PORT")
    <*> flag False True (long "debug")
    <*> flag False True (long "hello")
    <*> optional (option auto (long "add"))
    <*> optional (option auto (long "divide"))

serverProc :: RSIO () ()
serverProc = do
  liftIO $ putStrLn "waiting"
  d <- receiveData >>= forceDecodeParam @String Stage2
  logDebugN $ "received:" +|| d ||+ ""
  logDebugN "send"
  liftIO $ threadDelay $ 5 * 1000 * 1000
  sendData $ encode @String "World"
  logDebugN "done"

proc :: RSIO () ()
proc = do
  logDebugN "send"
  sendData $ encode @String "Hello"
  logDebugN "waiting"
  d <- decode @String <$> receiveData
  logDebugN $ "received:" +|| d ||+ ""
