{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Door.Door
  ( DoorState (..),
    Door (..),
    SDoorState(..),
    SomeDoor(..),
    SDoorStateI(..),
    mkDoor,
    withSomeDoor,

    closeDoor,
    openDoor,
    parseDoor,

    doorStateV1,
    toggleStateV1,
    openAnyDoorV1,

    doorStateV2,
    toggleStateV2,
    openAnyDoorV2,

    doorStateV3,
    toggleStateV3,
    openAnyDoorV3,

    doorState,
    toggleState,
    openAnyDoor,
    toggleStateSafe,
  )
where

import Data.Kind
import Data.Proxy
import Door.Common (DoorState(..))

type Door :: DoorState -> Type
data Door (s :: DoorState) where
  MkDoor :: Door s
  deriving (Show)

type SomeDoor :: Type
data SomeDoor where
  MkSomeDoor :: forall s. SDoorStateI s => Door s -> SomeDoor

instance Show SomeDoor where show (MkSomeDoor d) = show d

type SDoorState :: forall {a}. a -> Type
data SDoorState s where
  SOpened :: SDoorState 'Opened
  SClosed :: SDoorState 'Closed

class SDoorStateI s where 
  sDoorState :: SDoorState s
instance SDoorStateI 'Opened where sDoorState = SOpened
instance SDoorStateI 'Closed where sDoorState = SClosed

type WithSomeDoor a = forall c. SDoorStateI c => Door c -> a

withSomeDoor :: forall a. WithSomeDoor a -> SomeDoor -> a
withSomeDoor f (MkSomeDoor d) = f d

openDoor :: Door 'Closed -> Door 'Opened
openDoor _ = MkDoor

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor _ = MkDoor

parseDoor :: String -> Maybe SomeDoor
parseDoor "Opened" = Just $ MkSomeDoor (MkDoor @'Opened)
parseDoor "Closed" = Just $ MkSomeDoor (MkDoor @'Closed)
parseDoor _ = Nothing

toggleStateV1 :: forall s. KnownDoorState s => Door s -> SomeDoor
toggleStateV1 d = case doorStateVal @s Proxy of
  -- Opened -> MkSomeDoor $ closeDoor d     -- ERROR "Couldn't match type ‘s’ with ‘'Opened’ Expected: Door 'Opened Actual: Door s ‘s’ is a rigid type variable bound" 
  _ -> undefined

doorStateV1 :: forall s. KnownDoorState s => Door s -> DoorState
doorStateV1 _ = doorStateVal @s Proxy

openAnyDoorV1 :: forall s. KnownDoorState s => Door s -> Door 'Opened
openAnyDoorV1 d = case doorStateVal @s Proxy of
  -- Opened -> d
  -- Closed -> openDoor d
  _ -> undefined

doorStateV2 :: SDoorState s -> Door s -> DoorState
doorStateV2 sg _ = case sg of
  SOpened -> Opened
  SClosed -> Closed

openAnyDoorV2 :: SDoorState s -> Door s -> Door 'Opened
openAnyDoorV2 sg d = case sg of
  SOpened -> d
  SClosed -> openDoor d

toggleStateV2 :: SDoorState s -> Door s -> SomeDoor 
toggleStateV2 sg d = case sg of
                       SOpened -> MkSomeDoor $ closeDoor d
                       SClosed -> MkSomeDoor $ openDoor d

mkDoor :: SDoorState s -> Door s
mkDoor _ = MkDoor

toggleStateV3 :: forall s. SDoorStateI s => Door s -> SomeDoor 
toggleStateV3 = toS toggleStateV2

openAnyDoorV3 :: forall s. SDoorStateI s => Door s -> Door 'Opened
openAnyDoorV3 = toS openAnyDoorV2

doorStateV3 :: forall s. SDoorStateI s => Door s -> DoorState
doorStateV3 = toS doorStateV2

doorState :: forall s. SDoorStateI s => Door s -> DoorState
doorState = doorStateV3
openAnyDoor :: forall s. SDoorStateI s => Door s -> Door 'Opened
openAnyDoor = openAnyDoorV3
toggleState :: forall s. SDoorStateI s => Door s -> SomeDoor 
toggleState = toggleStateV3 

type family ToggledState d where
  ToggledState 'Opened = 'Closed
  ToggledState 'Closed = 'Opened

toggleStateSafe :: Door s -> Door (ToggledState s) 
toggleStateSafe MkDoor = MkDoor 

toS :: forall s r. SDoorStateI s => (SDoorState s -> r) -> r
toS f = f (sDoorState @s) 

class KnownDoorState s where
  doorStateVal :: Proxy s -> DoorState

-- with proxy
instance KnownDoorState 'Opened where doorStateVal _ = Opened

instance KnownDoorState 'Closed where doorStateVal _ = Closed

instance (KnownDoorState s) => KnownDoorState (Door s) where doorStateVal _ = doorStateVal (Proxy @s)
