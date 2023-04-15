-- Run
--    stack ghci -- src/typelevel/evenProof.hs
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Kind
import Data.Type.Dec
import Data.Type.Nat
import Data.Type.Equality
import Utils

snatPrev :: SNat ('S m) -> SNat m
snatPrev SS = snat

-- ======================================== Solution 1 ========================================
type IsEvenProof :: Nat -> Type
data IsEvenProof n where
  EReflZ :: IsEvenProof 'Z
  EReflSS :: IsEvenProof n -> IsEvenProof ('S ('S n))

decideEven :: SNat n -> Dec (IsEvenProof n)
decideEven SZ = Yes EReflZ
decideEven s@SS = case snatPrev s of
  SZ -> No $ \case {}
  t@SS -> case decideEven (snatPrev t) of
    Yes x -> Yes (EReflSS x)
    No rf -> No $ \case
      (EReflSS x) -> rf x

computeIfEven :: forall n. IsEvenProof n -> SNat n -> IO ()
computeIfEven _ n = putStrLn $ show (snatToNatural n) ++ " is even"

useSomeEven :: forall n. SNat n -> IO ()
useSomeEven n = case decideEven n of
                  Yes p -> computeIfEven p n
                  No _ -> putStrLn $ show (snatToNatural n) ++ " is ODD"

-- ======================================== Solution 2 ========================================

type IsEven :: Nat -> Bool
type family IsEven a where
  IsEven 'Z = 'True
  IsEven ('S 'Z) = 'False
  IsEven ('S ('S a)) = IsEven a

-- Solution 2
computeIfEvenT :: (IsEven n ~ 'True) => SNat n -> IO ()
computeIfEvenT n = putStrLn $ show (snatToNatural n) ++ " is even"

x = computeIfEvenT (SS @('S ('S 'Z)))

useSomeEvenT :: forall n. SNat n -> IO ()
useSomeEvenT n = case decideEven2 n of
                   Yes Refl -> computeIfEvenT n
                   No _ -> putStrLn $ show (snatToNatural n) ++ " is ODD"

decideEven2 :: SNat n -> Dec (IsEven n :~: 'True)
-- decideEven2 s = Refl
decideEven2 SZ = Yes Refl
decideEven2 n@SS = case snatPrev n of
                     SZ -> No $ \case{}
                     m@SS -> decideEven2 (snatPrev m)

type IsEvenC :: Nat -> Constraint
class IsEvenC n where
instance IsEvenC 'Z where
instance IsEvenC ('S ('S n)) where

-- decideEvenC :: SNat n -> Dec ((IsEvenC n => r) -> r)
-- decideEvenC SZ = Yes

-- reverseW :: IsEvenC ('S ('S n)) => SNat n -> (IsEvenC n => r) -> r
-- reverseW SZ f = f
-- reverseW s@SS f = f


test = do
  printBanner "Solution1"
  computeIfEven EReflZ (snat @'Z)
  -- computeIfEven ERefl (snat @Nat1)    -- ERROR "couldn't match type Odd with Even
  computeIfEven (EReflSS EReflZ) (snat @Nat2)
  useSomeEven (snat @'Z)
  useSomeEven (snat @Nat1)
  useSomeEven (snat @Nat2)
  useSomeEven (snat @Nat3)

  printBanner "Solution2"
  computeIfEvenT (snat @'Z)
  -- computeIfEven2 (snat @Nat1)    -- ERROR "couldn't match type 'Odd with 'Even
  computeIfEvenT (snat @Nat2)
  useSomeEvenT (snat @'Z)
  useSomeEvenT (snat @Nat1)
  useSomeEvenT (snat @Nat2)
  useSomeEvenT (snat @Nat2)
