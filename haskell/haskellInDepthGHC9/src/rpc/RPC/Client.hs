{-# LANGUAGE OverloadedStrings #-}
module RPC.Client
  ( requestRSIO,
    requestOperation,
    callRemote,
  )
where

import Data.Serialize
import Network.Connection
import RPC.Common
import RPC.Messaging

requestOperation :: (OperationDataType a, OperationDataType b) => RPCParams -> Operation -> a -> IO b
requestOperation rsParams op opArgs =
  requestRSIO rsParams (callRemote @() op opArgs)

requestRSIO :: RemoteState s => RPCParams -> RSIO s a -> IO a
requestRSIO rsParams comp = do
  ctx <- initConnectionContext
  conn <- connectTo ctx (connectionParams rsParams)
  (res, _) <- execRSIO (rsParams {connection = conn}) comp
  connectionClose conn
  return res

callRemote :: forall s a b. (OperationDataType a, OperationDataType b) => Operation -> RSAction s a b
callRemote op opArgs = do
  let packet = MkRPCPacket op $ encode opArgs
  logDebugN $ pretty packet
  logDebugN $ fmt $ "operation:"+|op|+" params" +||opArgs||+""
  sendData $ encode packet
  res <- receiveData >>= forceDecodeStage @RPCResult Stage1
  logDebugN $ pretty res
  case res of
    CallSuccess s -> forceDecodeStage @b Stage2 s
    CallFail e -> throwM e
