{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module TestClient
  ( echo,
    ping,
    reset,
    predictFuture,
    trivial,
    trivialForAll,
    unwrapAll,
    to3DPoint,
  )
where

import Common
import RPC.Base

[remote|
echo :: String -> RemoteObj String
ping :: RemoteObj Int
reset :: RemoteObj Int
trivial :: OperationDataType a => a -> RemoteObj a
to3DPoint :: Double -> Double -> Double -> RemoteObj (Double, Double, Double)
unwrapAll :: Maybe (Maybe (Either Int Double)) -> RemoteObj Double
trivialForAll :: forall a. OperationDataType a => a -> RemoteObj a
predictFuture :: RemoteObj String
  |]
