{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module SingletonBasics
  ( test,
  )
where

import Data.Kind
import Data.Singletons
import Data.Type.Nat
import Fmt
import SingletonCommon

type SomeCond :: forall {k}. k -> Constraint
class SomeCond a

instance SomeCond a

data Container where
  MkContainer :: forall a. TaggedData a -> Container

type TaggedData :: forall {k}. k -> Type
data TaggedData a where
  MkBoolData :: (SomeCond a, SingI a) => TaggedData (a :: Bool)
  MkNatData :: forall a. (SNatI a, SomeCond a) => TaggedData a

instance Show (TaggedData a) where
  show s@MkBoolData = "TaggedData Bool(" +|| getBoolTag s ||+ ")"
  show MkNatData = "TaggedData Nat(" +|| toNatural (reflect @a Proxy) ||+ ")"

instance Show (TaggedData a) => Buildable (TaggedData a) where build = build . show

instance Buildable Container where build = build . show

instance Show (TaggedData a) => Eq (TaggedData a) where x == y = show x == show y

instance Show Container where
  show (MkContainer s@MkBoolData) = "Container [" +| s |+ "]"
  show (MkContainer s@MkNatData) = "Container [" +| s |+ "]"

-- reflect: i.e. type -> value
getBoolTag :: forall a. TaggedData (a :: Bool) -> Bool
getBoolTag MkBoolData = withSing @a fromSing

-- reify i.e value -> type
mkBoolData :: Bool -> Container
mkBoolData v = case v of
  True -> MkContainer $ sn STrue
  False -> MkContainer $ sn SFalse
  where
    sn :: forall (s :: Bool). SBool s -> TaggedData s
    sn sg = withSingI sg MkBoolData

-- reflect: type (Nat) -> value (Int)
getNumTag :: forall a. TaggedData (a :: Nat) -> Int
getNumTag MkNatData = fromIntegral $ toNatural $ reflect @a Proxy

mkNumData :: Int -> Container
mkNumData 0 = MkContainer $ mkNumData' (snat @'Z)
mkNumData 1 = MkContainer $ mkNumData' (snat @Nat1)
mkNumData 2 = MkContainer $ mkNumData' (snat @Nat2)
mkNumData 3 = MkContainer $ mkNumData' (snat @Nat3)
mkNumData 4 = MkContainer $ mkNumData' (snat @Nat4)
mkNumData 5 = MkContainer $ mkNumData' (snat @Nat5)
mkNumData _ = undefined

-- reify: value (Int) -> type (Nat)
mkNumData' :: SNat s -> TaggedData s
mkNumData' n = withSNat n MkNatData

test :: IO ()
test = do
  let v1 = MkBoolData @'True
      -- v2 = MkTaggedData @('Just 'True)
      a1 = getBoolTag v1
      -- a2 = getBoolTag v2
      v1' = mkBoolData a1
      -- v2' = mkTaggedData2 a2       -- BAD. Shouldn't be able to change

      v3 = MkNatData @Nat1
      n3 = getNumTag v3
      v3' = mkNumData n3

  pretty $ nameF "v1" (build v1)
  pretty $ nameF "getBoolTag v1'" (build $ show a1)
  fmt $ nameF "mkBoolData $ getBoolTag v1'" (build v1')

  -- pretty $ nameF "v2" (build v2)
  -- pretty $ nameF "getBoolTag v2'" (build $ show a2)
  -- pretty $ nameF "mkBoolData $ getBoolTag v2'" (build v2')

  pretty $ nameF "v3" (build v3)
  pretty $ nameF "getNumTag v3" (build $ show n3)
  pretty $ nameF "mkNumData $ getNumTag v3" (build v3')
