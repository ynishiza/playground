{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter13.MyNat
  ( run,
    Nat (..),
    SNat (..),
    SNatI (..),
    N0,
    N1,
    N2,
    N3,
    N4,
    sn0,
    sn1,
    sn2,
    sn3,
    sn4,
  )
where

import Data.Kind
import Data.Proxy
import Fmt
import GHC.TypeLits qualified as T
import Utils

run :: TestState
run = createChapterTest "13" "singleton with Nat" (do
  p "N0" $ toNum (Proxy @N0)
  p "N1" $ toNum (Proxy @N1)
  p "N2" $ toNum (Proxy @N2)
  p "N3" $ toNum (Proxy @N3)
  p "N4" $ toNum (Proxy @N4)

  p "f 0 0" $ f sn0 sn0
  p "f 0 1" $ f sn0 sn1
  -- p "f 1 0" $ f sn1 sn0
  p "f 0 2" $ f sn1 sn1
  p "f 1 1" $ f sn1 sn2
  p "f 1 2" $ f sn1 sn3
  pure ()

  )
  where p n v = fmt $ nameF n (build $ show v)

type Nat :: Type
data Nat where
  Zero :: Nat
  Suc :: Nat -> Nat

type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat 'Zero
  SNat :: SNat a -> SNat ('Suc a)

instance (T.KnownNat (NatToNatural n)) => Show (SNat n) where
  show = show . stoNum

class SNatI (a :: Nat) where
  snat :: SNat a

instance SNatI 'Zero where snat = SZero

instance SNatI a => SNatI ('Suc a) where snat = SNat snat

class LENat (x :: Nat) (y :: Nat)

instance LENat 'Zero x

-- instance {-# OVERLAPPING #-} LENat x y => LENat x ('Suc y) where
instance LENat x y => LENat ('Suc x) ('Suc y)

type NatToNatural :: Nat -> T.Natural
type family NatToNatural (n :: Nat) where
  NatToNatural 'Zero = 0
  NatToNatural ('Suc s) = 1 T.+ NatToNatural s

type N0 = 'Zero

type N1 = 'Suc N0

type N2 = 'Suc N1

type N3 = 'Suc N2

type N4 = 'Suc N3

sn0 :: SNat N0
sn0 = snat @N0

sn1 :: SNat N1
sn1 = snat @N1

sn2 :: SNat N2
sn2 = snat @N2

sn3 :: SNat N3
sn3 = snat @N3

sn4 :: SNat N4
sn4 = snat @N4

stoNum :: forall n. (T.KnownNat (NatToNatural n)) => SNat n -> Integer
stoNum _ = T.natVal (Proxy @(NatToNatural n))

toNum :: forall p n. (T.KnownNat (NatToNatural n), SNatI n) => p n -> Integer
toNum _ = stoNum $ snat @n

f :: LENat x y => m x -> m y -> (m x, m y)
f = (,)
