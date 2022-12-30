{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestServer
  ( table,
  )
where

import Common
import Data.Serialize hiding (get, put)
import GHC.Real
import RPC.Base

echo :: String -> RemoteObj String
echo s = pure $ "echo:" +| s |+ ""

ping :: RemoteObj Int
ping = do
  modify (+ 1)
  get

reset :: RemoteObj Int
reset = do
  current <- get
  put 0
  return current

trivial :: a -> a
trivial = id

to3DPoint :: Double -> Double -> Double -> RemoteObj (Double, Double, Double)
to3DPoint x y z = pure (x, y * 2, z * 3)

trivialForAll :: forall a. a -> a
trivialForAll = id

unwrapAll :: Maybe (Maybe (Either Int Double)) -> RemoteObj Double
unwrapAll (Just (Just (Right x))) = pure x
unwrapAll _ = pure $ fromRational infinity

table :: RPCTable Int
table =
  [ ( "echo",
      forceDecodeParam Stage2
        >=> (encode <$>) . echo
    ),
    ("ping", const $ encode <$> ping),
    ("trivial", pure . trivial),
    ("trivialForAll", pure . trivialForAll),
    ("unwrapAll", forceDecodeParam Stage2 >=> (encode <$>) . unwrapAll),
    ( "to3DPoint",
      \d -> do
        (x, y, z) <- forceDecodeParam Stage2 d
        encode <$> to3DPoint x y z
    ),
    ("reset", const $ encode <$> reset)
  ]
