{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypePatternMatching where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data Value = A | B | C | D 
type Fn1 :: forall a. a -> Type
data Fn1 a
type Fn2 :: forall a. a -> Type
data Fn2 a
type (:+) :: forall a b. a -> b -> Type
data a :+ b

type ToSymbol :: forall a. a -> Symbol
type family ToSymbol a where
  ToSymbol 'A = "A"
  ToSymbol 'B = "B"
  ToSymbol 'C = "C"
  ToSymbol 'D = "D"
  ToSymbol (a :+ b) = AppendSymbol (ToSymbol a) (AppendSymbol " " (ToSymbol b))
  ToSymbol (Fn1 a) = AppendSymbol (AppendSymbol "Fn1(" (ToSymbol a)) ")"
  ToSymbol (Fn2 a) = AppendSymbol (AppendSymbol "Fn2(" (ToSymbol a)) ")"

type HasSecretCode :: forall a. a -> Constraint
class HasSecretCode a where
  getSecreteCode :: Proxy a -> Int 
instance HasSecretCode 'A where getSecreteCode _ = 1
instance HasSecretCode 'B where getSecreteCode _ = 2
instance HasSecretCode 'C where getSecreteCode _ = 3
instance HasSecretCode 'D where getSecreteCode _ = 4
instance HasSecretCode a => HasSecretCode (Fn1 a) where getSecreteCode _ = getSecreteCode (Proxy @a) * 10
instance HasSecretCode a => HasSecretCode (Fn2 a) where getSecreteCode _ = negate $ getSecreteCode (Proxy @a) 
instance (HasSecretCode a, HasSecretCode b) => HasSecretCode (a :+ b) where getSecreteCode _ = getSecreteCode (Proxy @a) + getSecreteCode (Proxy @b)

type V1 = 'A :+ 'B
type V2 = 'A :+ 'B :+ 'C :+ 'D
type V3 = (Fn1 (Fn2 (Fn1 'A)) :+ Fn1 V1) :+ Fn2 V1

testScratch = do
  putStrLn $ symbolVal (Proxy @(ToSymbol V1))
  putStrLn $ symbolVal (Proxy @(ToSymbol V2))
  putStrLn $ symbolVal (Proxy @(ToSymbol V3))

  print $ getSecreteCode (Proxy @V1)
  print $ getSecreteCode (Proxy @V2)
  print $ getSecreteCode (Proxy @V3)
