{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter13.SingletonProof
  ( EvenOdd,
    test,
    run,
  )
where

import Chapter13.Nat.Base
import Data.Kind
import Data.Singletons.TH
import Data.Type.Dec
import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Typeable
import Fmt
import Utils hiding (useIO)

snatPred :: forall n. SNat ('S n) -> SNat n
snatPred SS = snat @n

type EvenOdd :: Type
data EvenOdd = IsEven | IsOdd deriving (Show, Eq, Bounded, Enum)

genSingletons [''EvenOdd]

type SomeNat :: Type
data SomeNat where
  MkSomeNat :: forall (n :: Nat). SNat n -> SomeNat

prefixNat :: SNat s -> String -> Builder
prefixNat v msg = fmt $ snatToNatural v ||+ ":" +| msg |+ ""

suffixNat :: String -> SNat s -> Builder
suffixNat msg v = fmt $ snatToNatural v ||+ ":" +| msg |+ ""

processEvenNumber :: forall n. (EO n ~ 'IsEven) => SNat n -> IO ()
processEvenNumber s = fmtLn $ prefixNat s "EVEN"

processEvenNumber2 :: forall n. EvenProof n -> SNat n -> IO ()
processEvenNumber2 _ s = fmtLn $ prefixNat s "EVEN v2"

processSmallNumber :: forall n. LEProof n Nat5 -> SNat n -> IO ()
processSmallNumber _ s = fmtLn $ prefixNat s " <= 5"

processSome1 :: SomeNat -> IO ()
processSome1 (MkSomeNat (v :: SNat n)) = case decideEO v of
  SIsEven -> processEvenNumber v
  SIsOdd -> fmtLn $ prefixNat v "ODD EO"

processSome1b :: SomeNat -> IO ()
processSome1b (MkSomeNat (v :: SNat n)) = case decideEOWithInduction v of
  SIsEven -> processEvenNumber v
  SIsOdd -> fmtLn $ prefixNat v "ODD EO"

processSome2 :: SomeNat -> IO ()
processSome2 (MkSomeNat (s :: SNat n)) = withSNat s $ case decideLE @n @Nat5 of
  Yes pf -> processSmallNumber pf s
  No _ -> fmtLn $ prefixNat s ">5"

processSome3 :: SomeNat -> IO ()
processSome3 (MkSomeNat (s :: SNat n)) = withSNat s $ case decideEven4 s of
  Yes pf -> processEvenNumber2 pf s
  No _ -> fmtLn $ prefixNat s "ODD v2"

-- ==================== Double with induction ====================
--
type NDouble :: Nat -> Nat
type family NDouble n where
  NDouble 'Z = 'Z
  NDouble ('S n) = ('S ('S (NDouble n)))

data Hd n where
  MkHd :: SNat (NDouble n) -> Hd n

computeNDouble :: forall n. SNat n -> SNat (NDouble n)
computeNDouble n = case computeNDouble' n of (MkHd x) -> x

computeNDouble' :: forall n. SNat n -> Hd n
computeNDouble' SZ = MkHd SZ
computeNDouble' SS = induction (MkHd SZ) step
  where
    step :: forall m. Hd m -> Hd ('S m)
    step (MkHd SZ) = MkHd SS
    step (MkHd SS) = MkHd SS

-- ==================== solution 1: Even with type family EO ====================
type EO :: Nat -> EvenOdd
type family EO a where
  EO 'Z = 'IsEven
  EO ('S a) = InvertEvenOdd (EO a)
  EO _ = 'IsEven

type family InvertEvenOdd a = r | r -> a where
  InvertEvenOdd 'IsEven = 'IsOdd
  InvertEvenOdd 'IsOdd = 'IsEven

decideEO :: forall n. SNat n -> Sing (EO n)
decideEO sn = case sn of
  SZ -> SIsEven
  m@SS -> eoStep (snatPred m) $ decideEO (snatPred m)

eoStep :: forall a. SNat a -> Sing (EO a) -> Sing (EO ('S a))
eoStep _ SIsEven = SIsOdd
eoStep _ SIsOdd = SIsEven

decideEOWithInduction :: forall n. SNat n -> Sing (EO n)
decideEOWithInduction s = withEvenProof (evenInduction s) sing

withEvenProof :: forall n r. EOProof n -> ((SNatI n, SingI (EO n)) => r) -> r
withEvenProof MkEOProof f = f

evenInduction :: forall n. SNat n -> EOProof n
evenInduction s = withSNat s $
  induction (MkEOProof @'Z) $
    \(MkEOProof :: EOProof m) -> case (sing @(EO m)) of
      SIsEven -> MkEOProof
      SIsOdd -> MkEOProof

data EOProof n where
  MkEOProof :: (SingI (EO n), SNatI n) => EOProof n

-- ==================== solution 2: even with proof value ====================
-- solution: with Dec
type EvenProof :: Nat -> Type
data EvenProof n where
  EPZ :: EvenProof 'Z
  EPSS :: SNatI n => EvenProof n -> EvenProof ('S ('S n))

decideEven4 :: forall n. SNat n -> Dec (EvenProof n)
decideEven4 sn = case sn of
  -- case: n == 0
  SZ -> Yes EPZ
  SS -> case snatPred sn of
    -- case: n - 1 == 0
    SZ -> No $ \case {}
    -- case: n - 1 > 0
    y@SS -> step2 $ snatPred y
  where
    step2 :: forall m. SNat m -> Dec (EvenProof ('S ('S m)))
    step2 SZ = Yes $ EPSS EPZ
    step2 SS = case decideEven4 (snat @m) of
      Yes x -> Yes $ EPSS x
      No f -> No $ \case (EPSS n) -> f n

countN :: SNatI n => IO (SNat n)
countN = case countN' of (MKION x) -> x

countN' :: SNatI n => ION n
countN' = induction (MKION $ pure SZ) step
  where
    step :: forall m. SNatI m => ION m -> ION ('S m)
    step (MKION v) = MKION $ do
      v >>= fmtLn . suffixNat "v:"
      pure (snat @('S m))

newtype ION (n :: Nat) = MKION (IO (SNat n))

-- ==================== Nat list ====================
data SomeNatList (n :: Nat) = forall (l :: [Nat]). (Typeable n, Typeable l) => MkL (SNat n) (Proxy l)

buildNatList :: SNat n -> SomeNatList n
buildNatList x = withSNat x $ induction (MkL SZ (Proxy @('Z ': '[]))) step
  where
    step :: forall m. SNatI m => SomeNatList m -> SomeNatList ('S m)
    step (MkL _ (_ :: Proxy l)) = MkL snat (Proxy @('S m ': l))

instance Show (SomeNatList n) where show (MkL _ p) = showType p
instance {-# OVERLAPPABLE #-} Show a => Buildable a where build = build.show

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

run :: TestState
run = createChapterTest "13" "Proof with singletons" test

test :: IO ()
test = do
  processEvenNumber (snat @'Z)
  processEvenNumber (snat @Nat2)
  -- processEvenNumber (snat @Nat3)   -- error "Couldn't matach type IsOdd with IsEven"
  processEvenNumber (snat @Nat4)

  printBannerWrap "Even odd" $ do
    foldMap processSome1 someNums
    foldMap processSome1b someNums
    foldMap processSome2 someNums
    foldMap processSome3 someNums

  printBanner "NDouble"
  print $ suffixNat "double 0" $ computeNDouble SZ
  print $ suffixNat "double 1" $ computeNDouble (snat @MyNum1)
  print $ suffixNat "double 2" $ computeNDouble (snat @MyNum2)
  print $ suffixNat "double 100" $ computeNDouble (snat @MyNum100)

  countN @MyNum1 >>= fmtLn . suffixNat "countN"
  countN @MyNum2 >>= fmtLn . suffixNat "countN"

  printBannerWrap "Nat list" $ do
    fmt $ nameF "[0]" $ build $ show $ buildNatList (snat @MyNum0)
    fmt $ nameF "[2,1,0]" $ build $ show $ buildNatList (snat @MyNum2)
    fmt $ nameF "[20..0]" $ build $ show $ buildNatList (snat @MyNum20)
