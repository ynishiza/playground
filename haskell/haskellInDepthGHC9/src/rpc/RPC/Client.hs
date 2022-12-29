{-# LANGUAGE OverloadedStrings #-}
module RPC.Client
  ( requestBase,
    requestOperation,
  )
where

import Data.Serialize
import Network.Connection
import RPC.Common
import RPC.Messaging

requestOperation :: (OperationDataType a, OperationDataType b) => RPCParams -> Operation -> a -> IO b
requestOperation rsParams op opArgs =
  requestBase rsParams (callRemote @() op opArgs)

requestBase :: RemoteState s => RPCParams -> RSIO s a -> IO a
requestBase rsParams comp = do
  ctx <- initConnectionContext
  conn <- connectTo ctx (connectionParams rsParams)
  (res, _) <- execRSIO (rsParams {connection = conn}) comp
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
