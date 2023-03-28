-- Run
--    stack ghci -- src/typelevel/equalityProof.hs
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.Type.Dec
import Data.Type.Equality
import Data.Type.Nat
import Fmt

snatPrev :: SNat ('S m) -> SNat m
snatPrev SS = snat

eq :: SNat n -> SNat m -> Dec (n :~: m)
eq SZ SZ = Yes Refl
eq SZ SS = No $ \case {}
eq SS SZ = No $ \case {}
eq n1@SS n2@SS = withSNat p1 $ withSNat p2 $ case eq p1 p2 of
  Yes Refl -> Yes Refl
  No ref -> No $ \case
    x -> ref (rev x)
  where
    p1 = snatPrev n1
    p2 = snatPrev n2
    rev :: 'S n :~: 'S m -> n :~: m
    rev Refl = Refl

whenEq :: n ~ m => SNat n -> SNat m -> IO ()
whenEq n m = fmtLn $ snatToNatural n ||+ "==" +|| snatToNatural m ||+ ""

isTwo :: SNat n -> Dec (n :~: Nat2)
isTwo SZ = No $ \case {}
isTwo n@SS = case snatPrev n of
  SZ -> No $ \case {}
  n1@SS -> case snatPrev n1 of
    SZ -> Yes Refl
    SS -> No $ \case {}

computeIfEq :: SNat n -> SNat m -> IO()
computeIfEq n m = case eq n m of
                    Yes Refl -> whenEq n m
                    No _ -> putStrLn "not equal"

test :: IO ()
test = do
  computeIfEq (snat @Nat0) (snat @Nat1)
  computeIfEq (snat @Nat0) (snat @Nat2)
  computeIfEq (snat @Nat0) (snat @Nat3)
  computeIfEq (snat @Nat1) (snat @Nat1)
  computeIfEq (snat @Nat1) (snat @Nat2)
  computeIfEq (snat @Nat1) (snat @Nat3)
  computeIfEq (snat @Nat2) (snat @Nat1)
  computeIfEq (snat @Nat2) (snat @Nat2)
  computeIfEq (snat @Nat2) (snat @Nat3)
