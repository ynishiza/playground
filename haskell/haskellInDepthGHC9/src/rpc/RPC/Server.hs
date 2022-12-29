{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RPC.Server
  ( startSereverBase,
    startOperationServer,
    sampleTable,
  )
where

import Control.Exception (ArithException (..))
import Data.Serialize
import Network.Connection
import Network.Simple.TCP
import RPC.Common
import RPC.Messaging

startOperationServer :: forall s. RemoteState s => RPCParams -> RPCTable s -> IO ()
startOperationServer rsParams table = do
  pretty $ nameF "supported operations" $ blockListF' "-" build $ fst <$> table
  startSereverBase @s rsParams $ do
    res <- main `catch` handleError
    logDebugN $ pretty res
    sendData $ encode res
  where
    main = do
      packet@(MkRPCPacket op opArgs) <- receiveData >>= forceDecodeStage Stage1
      logDebugN $ pretty packet
      case lookup op table of
        (Just f) -> CallSuccess <$> f opArgs
        _ -> pure $ CallFail $ OperationNotFound op
    handleError :: SomeException -> RSIO s RPCResult
    handleError (SomeException e) = pure $ CallFail $ OperationCallFail $ show e

startSereverBase :: RemoteState s => RPCParams -> RSIO s () -> IO ()
startSereverBase rsParams comp = do
  let connParams@(ConnectionParams {..}) = connectionParams rsParams
  writeLog connParams "startServer"
  serve (Host connectionHostname) (show connectionPort) (processRequest rsParams comp)

processRequest :: RemoteState s => RPCParams -> RSIO s () -> (Socket, SockAddr) -> IO ()
processRequest rsParams comp (s, a) =
  ( do
      writeLog connParams $ "Init" +|| a ||+ ""
      ctx <- initConnectionContext
      con <- connectFromSocket ctx s connParams
      fst <$> execRSIO (rsParams {connection = con}) comp
  )
    `catch` handleError
  where
    connParams = connectionParams rsParams
    handleError :: RemoteException -> IO ()
    handleError e = do
      writeLog connParams $ "ERROR" +|| show e ||+ ""
      pure ()

writeLog :: ConnectionParams -> Builder -> IO ()
writeLog connParams msg = fmt $ "LOG:" +| connParams |+ "" +|| msg ||+ ""

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
        pure $ encode $ sum list
    ),
    ( "divide",
      \d -> do
        (x, y) <- forceDecodeParam @(Double, Double) Stage2 d
        when (y == 0) (throwM DivideByZero)
        pure $ encode $ x / y
    )
  ]
