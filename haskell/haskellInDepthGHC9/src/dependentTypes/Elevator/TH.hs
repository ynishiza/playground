{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Elevator.TH
  ( genFloors,
    module X,
  )
where

import Data.Type.Nat
import Elevator.Elevator as X
import Language.Haskell.TH

maxNameS :: String
maxNameS = "MaxFloor"

numNameS :: String
numNameS = "MyNum"

mkNmName :: Show a => a -> Name
mkNmName n = mkName $ numNameS ++ show n

genFloors :: Int -> Q [Dec]
genFloors n
  | n <= 0 = fail "Must be a positive number"
  | otherwise = do
      ns <- foldMap genNumAliases [0 .. n]
      maxName <- genMaxFloor n
      fs <- foldMap genFloor [0 .. n]
      return $ ns ++ maxName ++ fs

genNumAliases :: Int -> Q [Dec]
genNumAliases n = do
  let name = mkNmName n
      sig =
        if n == 0
          then [t|'Z|]
          else [t|'S $(conT $ mkNmName (n - 1))|]
  dec <- tySynD name [] sig
  return [dec]

genMaxFloor :: Int -> Q [Dec]
genMaxFloor n = do
  let mxName = mkName maxNameS
      numName = mkNmName n
  dec <- tySynD mxName [] (conT numName)
  return [dec]

genFloor :: Int -> Q [Dec]
genFloor n = do
  let floorName = mkName $ "f" ++ show n
      numName = mkNmName n
      maxName = mkName maxNameS
      sig = [t|Floor $(conT maxName) $(conT numName)|]
      expr = [|MkFloor|]
  sigDec <- sigD floorName sig
  expDec <- valD (varP floorName) (normalB expr) []
  return [sigDec, expDec]
