{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE StandaloneKindSignatures #-} 
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TemplateHaskell #-} 

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

genSingletons [''Bool]

notT_ :: forall (a :: Bool) (b :: Bool). Sing a -> Sing b
notT_ = undefined
-- notT_ STrue = SFalse


-- solution 1: with type family
type Not :: Bool -> Bool
type family Not b where
  Not 'True = 'False
  Not 'False = 'True

notT :: forall (a :: Bool). Sing a -> Sing (Not a)
notT STrue = SFalse
notT SFalse = STrue


