{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Door.Common
  ( DoorState (..),
    SDoorState (..),
    SingI (..),
    OpenedSym0,
    ClosedSym0,
    module X,
  )
where

import Data.Singletons.TH
import Data.Singletons as X

$( singletons
     [d|
       data DoorState where
         Opened :: DoorState
         Closed :: DoorState
       |]
 )

deriving instance Show DoorState

deriving instance Eq DoorState

deriving instance Bounded DoorState

deriving instance Enum DoorState
