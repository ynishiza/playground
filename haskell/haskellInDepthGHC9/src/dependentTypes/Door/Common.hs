{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Door.Common
  ( DoorState (..),
    SDoorState (..),
    SingI (..),
    OpenSym0,
    ClosedSym0,
  )
where

import Data.Singletons.TH

$( singletons
     [d|
       data DoorState where
         Open :: DoorState
         Closed :: DoorState
       |]
 )

deriving instance Show DoorState

deriving instance Eq DoorState

deriving instance Bounded DoorState

deriving instance Enum DoorState
