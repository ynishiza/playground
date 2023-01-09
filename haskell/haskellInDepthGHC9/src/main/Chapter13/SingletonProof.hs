{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter13.SingletonProof
  ( EvenOdd,
    test,
  )
where

import Data.Kind
import Data.Singletons.TH
import Data.Type.Dec
import Data.Type.Nat
import Data.Type.Nat.LE
import Chapter13.Nat.Base
import Fmt

type EvenOdd :: Type
data EvenOdd = IsEven | IsOdd deriving (Show, Eq, Bounded, Enum)

genSingletons [''EvenOdd]

type EO :: Nat -> EvenOdd
type family EO a where
  EO 'Z = 'IsEven
  EO ('S a) = InvertEvenOdd (EO a)
  EO _ = 'IsEven

type family InvertEvenOdd a = r | r -> a where
  InvertEvenOdd 'IsEven = 'IsOdd
  InvertEvenOdd 'IsOdd = 'IsEven

type SomeNat :: Type
data SomeNat where
  MkSomeNat :: forall (n :: Nat). SNat n -> SomeNat

showNat :: SNat s -> String -> Builder
showNat v msg = fmt $ snatToNatural v ||+":"+|msg|+""

processEvenNumber :: forall n. (EO n ~ 'IsEven) => SNat n -> IO ()
processEvenNumber s = fmtLn $ showNat s "EVEN"

processSmallNumber :: forall n. LEProof n Nat5 -> SNat n -> IO ()
processSmallNumber _ s = fmtLn $ showNat s " <= 5"

processSome1 :: SomeNat -> IO ()
processSome1 (MkSomeNat (v :: SNat n)) = case decideEven v of
  SIsEven -> processEvenNumber v
  SIsOdd -> fmtLn $ showNat v "ODD"

processSome2 :: SomeNat -> IO ()
processSome2 (MkSomeNat (s :: SNat n)) = withSNat s $ case decideLE @n @Nat5 of
  Yes pf -> processSmallNumber pf s
  No _ -> fmtLn $ showNat s ">5"

decideEven :: forall n. SNat n -> Sing (EO n)
decideEven s = case s of
  SZ -> SIsEven
  SS -> g s
  where
    g :: forall m. SNat ('S m) -> Sing (EO ('S m))
    g SS = step (snat @m) $ decideEven (snat @m)
    step :: forall a. SNat a -> Sing (EO a) -> Sing (EO ('S a))
    step _ SIsEven = SIsOdd
    step _ SIsOdd = SIsEven

someNums :: [SomeNat]
someNums =
  [ MkSomeNat (snat @MyNum0),
    MkSomeNat (snat @MyNum1),
    MkSomeNat (snat @MyNum2),
    MkSomeNat (snat @MyNum3),
    MkSomeNat (snat @MyNum4),
    MkSomeNat (snat @MyNum5),
    MkSomeNat (snat @MyNum6),
    MkSomeNat (snat @MyNum7),
    MkSomeNat (snat @MyNum8),
    MkSomeNat (snat @MyNum9),
    MkSomeNat (snat @MyNum10),
    MkSomeNat (snat @MyNum11),
    MkSomeNat (snat @MyNum12),
    MkSomeNat (snat @MyNum13),
    MkSomeNat (snat @MyNum14),
    MkSomeNat (snat @MyNum15),
    MkSomeNat (snat @MyNum50),
    MkSomeNat (snat @MyNum51),
    MkSomeNat (snat @MyNum99),
    MkSomeNat (snat @MyNum100)
  ]

test :: IO ()
test = do
  processEvenNumber (snat @'Z)
  processEvenNumber (snat @Nat2)
  -- processEvenNumber (snat @Nat3)   -- error "Couldn't matach type IsOdd with IsEven"
  processEvenNumber (snat @Nat4)

  foldMap processSome1 someNums
  foldMap processSome2 someNums

-- p1 (snat @Nat5)   -- error "Couldn't matach type IsOdd with IsEven"
