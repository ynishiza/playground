{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

genSingletons [''Bool]

main :: IO ()
main = do
  pure ()

type MyData :: Bool -> Type
data MyData a where
  MkMyData :: MyData a

-- Bad
mkMyDataBad :: Bool -> MyData a
-- mkMyDataBad True = MkMyData @'True
mkMyDataBad False = withSingI SFalse MkMyData

x1 = mkMyDataBad @'True True

x2 = mkMyDataBad @'False True

-- solution 1: with witness
mkMyDataS :: Sing a -> MyData a
mkMyDataS s = withSingI s MkMyData

-- solution 2: with opaque container
data SomeData where
  MkSomeData :: forall (a :: Bool). MyData a -> SomeData

mkMyData :: Bool -> SomeData
mkMyData v = withSomeSing v $
  \(s :: SBool d) -> withSingI s $
      MkSomeData (MkMyData @d)
