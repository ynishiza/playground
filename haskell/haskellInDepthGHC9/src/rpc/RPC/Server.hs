{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RPC.Server
  ( startSereverBase,
    startOperationServer,
  )
where

import Data.Serialize
import Network.Connection
import Network.Simple.TCP
import Control.Monad
import RPC.Common
import RPC.Messaging

startOperationServer :: forall s. RemoteState s => RPCParams -> RPCTable s -> IO ()
startOperationServer rsParams table = do
  pretty $ nameF "supported operations" $ blockListF' "-" build $ fst <$> table
  startSereverBase @s rsParams $ do
    main
      `catches` [ Handler handleConnectionError,
                  Handler (handleError @SomeException)
                ]
  where
    logAndSend x = logDebugN (pretty x) >> sendData (encode x)
    main = do
      packet@(MkRPCPacket op opArgs) <- receiveData >>= forceDecodeStage Stage1
      logDebugN $ pretty packet
      res <- case lookup op table of
        (Just f) -> CallSuccess <$> f opArgs
        _ -> pure $ CallFail $ OperationNotFound op
      logAndSend res

    -- note: ignore ConnectionClosed (for now)
    -- This happens every time the client closes connection, even on success.
    handleConnectionError ConnectionClosed = throwM ConnectionClosed
    handleConnectionError e =
      logAndSend $ CallFail $ OperationCallFail $ show e
    handleError :: Exception e => e -> RSIO s ()
    handleError e =
      logAndSend $ CallFail $ OperationCallFail $ show e

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
      _ <- fst <$> execRSIO (rsParams {connection = con}) (forever comp)
      connectionClose con
  )
    `catch` handleError
  where
    connParams = connectionParams rsParams
    handleError :: RemoteException -> IO ()
    handleError e = do
      writeLog connParams $ "ERROR" +|| show e ||+ ""
      pure ()

writeLog :: ConnectionParams -> Builder -> IO ()
writeLog connParams msg = prettyLn @Builder $ "LOG:" +| connParams |+ "" +|| msg ||+ ""
