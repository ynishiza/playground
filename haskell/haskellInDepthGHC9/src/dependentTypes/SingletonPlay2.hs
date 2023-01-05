{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE PolyKinds #-} 
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE StandaloneKindSignatures #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# OPTIONS_GHC -Wno-orphans #-}

module SingletonPlay2 (
  v1,
  v2,
  v3,
  e1,
  e2,
  e3,
  ) where

import Data.Singletons
import Data.Type.Nat
import SingletonCommon()

data TaggedData a where
  -- MkTaggedData :: (SingKind k, SingI (a :: k)) => TaggedData a
  MkTaggedData :: forall a. TaggedData a

instance (Show (Sing a), SingI a) => Show (TaggedData a) where show _ = "TaggedData" ++ show (sing @a)
instance Show (TaggedData a) => Eq (TaggedData a) where x == y = show x == show y
instance {-# OVERLAPPING #-} SNatI a => Show (TaggedData a) where show MkTaggedData = show $ toNatural $ reflect @a Proxy

-- reflect: i.e. type -> value
getTag :: forall k (a :: k). (SingKind k, SingI a) => TaggedData a -> Demote k
getTag MkTaggedData = (withSing @a) fromSing

-- reify i.e value -> type
mkTaggedData :: forall k (a :: k). (SingKind k) => Demote k -> TaggedData a
mkTaggedData v = case toSing v of (SomeSing v') -> withSingI v' MkTaggedData

getNumTag :: forall a. SNatI a => TaggedData a -> Int
getNumTag MkTaggedData = fromIntegral $ toNatural $ reflect @a Proxy

mkNumData :: forall a. Int -> TaggedData (a :: Nat)
mkNumData n = reify (fromNatural $ fromIntegral n) (const MkTaggedData)

v1 = MkTaggedData @'True
v2 = MkTaggedData @('Just 'True)
a1 = getTag v1
a2 = getTag v2
a1' = mkTaggedData a1
a2' = mkTaggedData a2
e1 = v1 == a1'
e2 = v2 == a2'

v3 = MkTaggedData @Nat1
n3 = getNumTag v3
v3' = mkNumData n3
e3 = v3 == v3'
