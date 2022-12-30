{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}

import Network.Connection
import Network.Socket (PortNumber)
import Options.Applicative
import RPC.Client qualified as Client
import RPC.Messaging
import qualified RPC.Samples as RS
import RPC.Server qualified as Server

main :: IO ()
main = do
  argParams@(MkParams {..}) <- execParser parserInfo
  fmtLn $ nameF "CLI rsParams" (build argParams)
  let rsParams =
        MkRPCParams
          { 
            sourceName = if server then "server" else "client",
            logLevel = if debug then LevelDebug else LevelInfo,
            connectionParams = ConnectionParams host port Nothing Nothing,
            connection = undefined
          }
  case () of
    ()
      | server -> Server.startOperationServer rsParams (RS.sampleTable @())
      -- | hello -> Client.requestOperation rsParams "hello" () >>= print @String
      | hello -> Client.requestRSIO rsParams RS.hello >>= print @String
      -- | (Just p) <- add -> Client.requestOperation rsParams "add" p >>= print @Int
      | (Just p) <- add -> Client.requestRSIO rsParams (RS.add (fst p) (snd p)) >>= print @Int
      -- | (Just p) <- divide -> Client.requestOperation rsParams "divide" p >>= print @Double
      | (Just p) <- divide -> Client.requestRSIO rsParams (RS.divide (fst p) (snd p)) >>= print @Double

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

