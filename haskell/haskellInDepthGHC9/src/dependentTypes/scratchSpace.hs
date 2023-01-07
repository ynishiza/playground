{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}

import Data.Kind
import Data.Singletons
import SingletonCommon

type MyData :: Bool -> Type
data MyData a where
  MkMyData :: MyData a

-- Bad
mkMyDataBad :: Bool -> MyData a
mkMyDataBad True = withSingI STrue MkMyData
mkMyDataBad False = withSingI SFalse MkMyData

x1 = mkMyDataBad @'True True

x2 = mkMyDataBad @'False True

-- solution 1: with witness
mkMyDataS :: Sing Bool -> MyData a
mkMyDataS = flip withSingI MkMyData

-- solution 2: with opaque container
data SomeData where
  MkSomeData :: forall (a :: Bool). MyData a -> SomeData

mkMyData :: Bool -> SomeData
mkMyData True = MkSomeData (MkMyData @'True)
mkMyData False = MkSomeData (MkMyData @'False)
