{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Door.Common
  ( DoorState (..),
    SDoorState (..),
    SDoorStateI,
    SingI (..),
    OpenedSym0,
    ClosedSym0,
    module X,
  )
where

import Data.Singletons as X
import Data.Singletons.TH

$( singletons
     [d|
       data DoorState where
         Opened :: DoorState
         Closed :: DoorState
       |]
 )

type SDoorStateI s = SingI (s :: DoorState)

deriving instance Show DoorState

deriving instance Eq DoorState

deriving instance Bounded DoorState

deriving instance Enum DoorState
