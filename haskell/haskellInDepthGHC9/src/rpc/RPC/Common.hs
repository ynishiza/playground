{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RPC.Common
  ( RemoteException (..),
    RSIO (..),
    RPCParams (..),
    defaultRPCParams,
    DecodeStage (..),
    RemoteState (..),
    RPCTable,
    RPCPacket (..),
    RPCResult (..),
    Operation,
    RSAction,
    execRSIO,
    module X,
    logByteString,
    OperationDataType,
  )
where

import Control.Monad.Catch as X
import Control.Monad.Logger as X
import Control.Monad.Reader as X
import Control.Monad.State as X
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Data
import Data.Serialize
import Fmt as X
import GHC.Generics
import Network.Connection
import RPC.Exception as X

type RSAction s a b = a -> RSIO s b

type Operation = String

type OperationDataType a = (Serialize a, Show a)

data RPCPacket where
  MkRPCPacket ::
    { operation :: Operation,
      operationData :: ByteString
    } ->
    RPCPacket
  deriving stock (Generic, Eq)
  deriving anyclass (Serialize)

instance Buildable RPCPacket where build = build . show

instance Show RPCPacket where
  show (MkRPCPacket op d) = "operation:" +| op |+ " data:" +| logByteString d |+""

type RPCTable s = [(Operation, RSAction s ByteString ByteString)]

data RPCResult where
  CallSuccess :: ByteString -> RPCResult
  CallFail :: RPCException -> RPCResult
  deriving stock (Eq, Generic)
  deriving anyclass (Serialize)

logByteString :: B.ByteString -> Builder
logByteString x = B.length x ||+ "B"

instance Show RPCResult where
  show (CallSuccess d) = showObject "CallSuccess" $ pretty $ logByteString d
  show (CallFail e) = showObject "CallFail" $ show e

instance Buildable RPCResult where build = build . show

newtype RSIO s a = MkRSIO
  { runRSIO :: StateT s (ReaderT Connection (LoggingT IO)) a
  }
  deriving stock (Generic, Typeable)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState s,
      MonadReader Connection,
      MonadIO,
      MonadLogger,
      MonadLoggerIO,
      MonadThrow,
      MonadCatch
    )

data RPCParams where
  MkRPCParams ::
    { logLevel :: !LogLevel,
      connectionParams :: ConnectionParams,
      connection :: Connection
    } ->
    RPCParams

instance Show RPCParams where
  show (MkRPCParams lv (ConnectionParams host port _ _) _) = fmt $ "log level:" +|| lv ||+ " "
    <> host|+":"+||port||+""

defaultRPCParams :: RPCParams
defaultRPCParams =
  MkRPCParams
    { logLevel = LevelDebug,
      connectionParams = ConnectionParams "0.0.0.0" 12345 Nothing Nothing,
      connection = undefined
    }

data DecodeStage = Stage0 | Stage1 | Stage2 deriving (Show, Eq, Enum)

class RemoteState a where
  initState :: a

instance RemoteState () where
  initState = ()

execRSIO :: RemoteState s => RPCParams -> RSIO s a -> IO (a, s)
execRSIO param@(MkRPCParams {..}) (MkRSIO comp) =
  runStdoutLoggingT $
    p
      >> filterLogger
        (\_ l -> l >= logLevel)
        ( runReaderT (runStateT comp initState) connection
        )
  where
    p = logDebugN $ pretty (build $ show param)

instance Buildable ConnectionParams where
  build (ConnectionParams host port _ _) = host ||+ ":" +|| port ||+ ""
