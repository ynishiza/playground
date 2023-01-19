{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC  -Werror=incomplete-patterns #-}

import Data.Void
import Data.Proxy
import Data.Type.Equality
import Data.Type.Dec

data Nat where
  Z :: Nat
  SS :: Nat -> Nat

e :: Dec (Int :~: Bool)
e = No $ \case

e2 :: Dec (Neg (Int :~: Bool))
e2 = Yes $ \case

v :: Neg (Int :~: Bool)
v = \case
