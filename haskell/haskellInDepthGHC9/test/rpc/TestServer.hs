{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module TestServer
  ( 
  manualTable,
  generatedTable,
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

trivial :: forall a. a -> RemoteObj a
trivial = pure 

to3DPoint :: Double -> Double -> Double -> RemoteObj (Double, Double, Double)
to3DPoint x y z = pure (x, y * 2, z * 3)

trivialForAll :: forall a. a -> RemoteObj a
trivialForAll = pure 

unwrapAll :: Maybe (Maybe (Either Int Double)) -> RemoteObj Double
unwrapAll (Just (Just (Right x))) = pure x
unwrapAll _ = pure $ fromRational infinity

declareRPCTable "genTableBase" ['echo, 'ping, 'reset, 'to3DPoint, 'unwrapAll]
genTableBase :: RPCTable Int

generatedTable :: RPCTable Int
generatedTable = genTableBase ++ [
    -- note: any functions with parameterized variables need to be generated manually 
    -- since we can't tell how to serialize/deserialize the value. 
    ("trivial", trivial),
    ("trivialForAll",  trivialForAll)
                           ]

manualTable :: RPCTable Int
manualTable =
  [ ( "echo",
      forceDecodeParam Stage2
        >=> (encode <$>) . echo
    ),
    ("ping", const $ encode <$> ping),
    ("trivial", trivial),
    ("trivialForAll",  trivialForAll),
    ("unwrapAll", forceDecodeParam Stage2 >=> (encode <$>) . unwrapAll),
    ("unwrapAll", \d -> forceDecodeParam Stage2 d >>= unwrapAll >>= pure . encode),
    ( "to3DPoint",
      \d -> do
        (x, y, z) <- forceDecodeParam Stage2 d
        encode <$> to3DPoint x y z
    ),
    ("reset", const $ encode <$> reset)
  ]
