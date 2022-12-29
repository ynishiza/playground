{-# LANGUAGE OverloadedStrings #-}

module RPC.Messaging
  ( sendData,
    receiveData,
    forceDecodeParam,
    forceDecodeStage,
    module X,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize
import Data.Word
import Network.Connection
import RPC.Common as X
import System.IO.Error

type MsgHeader = Word64

msgHeaderBytes :: Int
msgHeaderBytes = 8

forceDecodeParam :: (OperationDataType a, MonadThrow m, MonadLogger m) => DecodeStage -> ByteString -> m a
forceDecodeParam stage s = do
  logDebugN $ "Decoding:"+| logByteString s|+""
  res <- forceDecodeStage stage s
  logDebugN $ "Decoded:" +||res||+""
  return res

forceDecodeStage :: (OperationDataType a, MonadThrow m) => DecodeStage -> ByteString -> m a
forceDecodeStage stage s = either (throwM . err) pure (decode s)
  where
    err msg = RemoteException $ "Failed to decode at stage " +|| stage ||+ " with error:" +|| msg ||+ ""

sendData :: ByteString -> RSIO s ()
sendData payload = do
  conn <- ask
  liftIO $ connectionPut conn msg
  logDebugN $ "Sent message " +| logByteString msg |+ ""
  where
    l = B.length payload
    msg = runPut $ do
      putByteString $ encode @MsgHeader (fromIntegral l)
      putByteString payload

receiveData :: RSIO s ByteString
receiveData = do
  conn <- ask
  msgSize <- liftIO (recvE conn msgHeaderBytes) >>= forceDecodeStage @MsgHeader Stage0
  res <- liftIO $ recvE conn (fromIntegral msgSize)
  logDebugN $ "Received message " +| logByteString res |+ ""
  return res
  where
    recvE conn l = connectionGet conn l `catch` handleGetError

handleGetError :: MonadThrow m => IOError -> m a
handleGetError e
  | isEOFError e = throwM ConnectionClosed
  | otherwise = throwM $ RemoteException $ displayException e
