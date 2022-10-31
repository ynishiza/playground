module TestExport (
  -- MyADT,
  -- MyADT(ADTA),
  MyADT(..),
  MyType,
  createA,
  createB,

  -- MyNewType,
  MyNewType(..),
  createMyNewType,

  -- MyClass,
  -- MyClass(myClassFn),
  MyClass(..),
  x,
  y,
) where

import GHC.Types

x :: Integer
x = 1

data MyADT a = ADTA a | ADTB a

createA :: a -> MyADT a
createA = ADTA
createB :: a -> MyADT a
createB = ADTB

type MyType (a::Type) = [a]

newtype MyNewType (a::Type -> Type) b = MkMyNewType (a b)
createMyNewType :: a b -> MyNewType a b
createMyNewType = MkMyNewType 

class MyClass (m::Type -> Type) where
  myClassFn:: m a -> a -> b -> m b

y :: Integer
y = TestExport.x
