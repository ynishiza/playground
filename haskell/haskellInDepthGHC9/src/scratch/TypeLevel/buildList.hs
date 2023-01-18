{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind
import Data.Proxy
import Data.Type.Nat
import Fmt
import Utils

-- setup: type of the list
type NatList :: Nat -> [Nat]
type family NatList n where
  NatList 'Z = '[]
  NatList ('S n) = 'S n ': NatList n

-- setup: witness for building the list
type ListBuilder :: Nat -> Type
data ListBuilder n where
  MkListBuilder :: Proxy (NatList n) -> ListBuilder n

-- main: build list
buildNatList :: forall n. SNat n -> Proxy (NatList n)
buildNatList n = l
  where
    (MkListBuilder l) = res
    res :: ListBuilder n
    res = withSNat n $ induction (MkListBuilder (Proxy @'[])) step
    step :: forall m. ListBuilder m -> ListBuilder ('S m)
    step (MkListBuilder (_ :: Proxy l)) = MkListBuilder (Proxy @('S m ': l))

l0 :: Proxy (NatList 'Z)
l0 = buildNatList (snat @'Z)

l1 :: Proxy '[Nat1]
l1 = buildNatList (snat @Nat1)

l2 :: Proxy '[Nat2, Nat1]
l2 = buildNatList (snat @Nat2)

l3 :: Proxy '[Nat5, Nat4, Nat3, Nat2, Nat1]
l3 = buildNatList (snat @Nat5)

test :: IO ()
test = do
  printBannerWrap "Nat list" $ do
    fmt $ nameF "[0]" $ build $ show $ buildNatList (snat @Nat0)
    fmt $ nameF "[2,1,0]" $ build $ show $ buildNatList (snat @Nat1)
    fmt $ nameF "[20..0]" $ build $ show $ buildNatList (snat @Nat2)
