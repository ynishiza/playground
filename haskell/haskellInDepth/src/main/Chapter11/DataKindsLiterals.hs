{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Chapter11.DataKindsLiterals () where

import GHC.Base
import GHC.Natural

b0 :: Bool
b0 = True
b1 :: Bool
b1 = False
type B0 :: Bool
type B0 = 'True
type B1 :: Bool
type B1 = 'False

t0 :: Natural
t0 = 0
t1 :: Natural
t1 = 1
type T0 :: Nat
type T0 = 0
-- type T0' :: Int
-- type T0' = 0          -- ERROR Expected kind ‘Int’, but ‘0’ has kind ‘Nat’ 
type T1 :: Nat
type T1 = 1

s0 :: String
s0 = ""
s1 :: String
s1 = "ABC"
type S0 :: Symbol
type S0 = ""
-- type S0' :: String
-- type S0' = ""           -- ERROR "Expected kind ‘String’, but ‘""’ has kind ‘Symbol’"
type S1 :: Symbol
type S1 = "ABC"
