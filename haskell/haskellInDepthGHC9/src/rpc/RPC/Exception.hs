{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.Exception (
  RemoteException(..),
  RPCException(..),
  showObject,
  ) where

import Data.Serialize
import Control.Exception
import Fmt 
import GHC.Generics

data RemoteException where
  ConnectionClosed :: RemoteException
  RemoteException :: String -> RemoteException
  deriving stock (Eq, Generic)
  deriving anyclass Exception

instance Show RemoteException where
  show ConnectionClosed = "Connection was closed"
  show (RemoteException s) = pretty $ nameF "RemoteException" (build s)

data RPCException where
  OperationNotFound :: String -> RPCException
  OperationCallFail :: String -> RPCException
  deriving stock (Eq, Generic)
  deriving anyclass (Serialize, Exception)

instance Show RPCException where
  show (OperationNotFound name) = showObject "OperationNotFound " $ fmt $ nameF "operation" (build name)
  show (OperationCallFail msg) = showObject "OperationCallFail " $ fmt $ nameF "error" (build msg)

showObject :: String -> String -> String
showObject name msg = pretty $ nameF (build name) (build msg)

