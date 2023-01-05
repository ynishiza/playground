{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Elevator.FloorTH
  ( genFloors,
  genNumAliases,
    module X,
  )
where

import Data.Type.Nat
import Elevator.Floor as X
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
      maxName <- genMaxFloor n
      fs <- foldMap (flip genFloor n) [0 .. n]
      return $ maxName ++ fs

genNumAliases :: Int -> Int -> Q [Dec]
genNumAliases l h = foldMap genNumAlias [l..h]

genNumAlias :: Int -> Q [Dec]
genNumAlias n = do
  let name = mkNmName n
      sig =
        if n == 0
          then [t|'Z|]
          else [t|'S $(conT $ mkNmName (n - 1))|]
  dec <- tySynD name [] sig
  return [dec]

genMaxFloor :: Int -> Q [Dec]
genMaxFloor n = do
  let mxName = mkName $ maxNameS ++ "_" ++ show n
      numName = mkNmName n
  dec <- tySynD mxName [] (conT numName)
  return [dec]

genFloor :: Int -> Int -> Q [Dec]
genFloor n mx = do
  let floorName = mkName $ "f" ++ show n ++ "_" ++ show mx
      numName = mkNmName n
      maxName = mkName $ maxNameS ++ "_" ++ show mx
      sig = [t|Floor $(conT maxName) $(conT numName)|]
      expr = [|MkFloor|]
  sigDec <- sigD floorName sig
  expDec <- valD (varP floorName) (normalB expr) []
  return [sigDec, expDec]
