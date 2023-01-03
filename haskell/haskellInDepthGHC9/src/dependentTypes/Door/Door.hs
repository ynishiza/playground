{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

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
  SOpen :: SDoorState 'Open
  SClosed :: SDoorState 'Closed

class SDoorStateI s where 
  sDoorState :: SDoorState s
instance SDoorStateI 'Open where sDoorState = SOpen
instance SDoorStateI 'Closed where sDoorState = SClosed

type WithSomeDoor a = forall c. SDoorStateI c => Door c -> a

withSomeDoor :: forall a. WithSomeDoor a -> SomeDoor -> a
withSomeDoor f (MkSomeDoor d) = f d

openDoor :: Door 'Closed -> Door 'Open
openDoor _ = MkDoor

closeDoor :: Door 'Open -> Door 'Closed
closeDoor _ = MkDoor

parseDoor :: String -> Maybe SomeDoor
parseDoor "Open" = Just $ MkSomeDoor (MkDoor @'Open)
parseDoor "Closed" = Just $ MkSomeDoor (MkDoor @'Closed)
parseDoor _ = Nothing

toggleStateV1 :: forall s. KnownDoorState s => Door s -> SomeDoor
toggleStateV1 d = case doorStateVal @s Proxy of
  -- Open -> MkSomeDoor $ closeDoor d     -- ERROR "Couldn't match type ‘s’ with ‘'Open’ Expected: Door 'Open Actual: Door s ‘s’ is a rigid type variable bound" 
  _ -> undefined

doorStateV1 :: forall s. KnownDoorState s => Door s -> DoorState
doorStateV1 _ = doorStateVal @s Proxy

openAnyDoorV1 :: forall s. KnownDoorState s => Door s -> Door 'Open
openAnyDoorV1 d = case doorStateVal @s Proxy of
  -- Open -> d
  -- Closed -> openDoor d
  _ -> undefined

doorStateV2 :: SDoorState s -> Door s -> DoorState
doorStateV2 sg _ = case sg of
  SOpen -> Open
  SClosed -> Closed

openAnyDoorV2 :: SDoorState s -> Door s -> Door 'Open
openAnyDoorV2 sg d = case sg of
  SOpen -> d
  SClosed -> openDoor d

toggleStateV2 :: SDoorState s -> Door s -> SomeDoor 
toggleStateV2 sg d = case sg of
                       SOpen -> MkSomeDoor $ closeDoor d
                       SClosed -> MkSomeDoor $ openDoor d

mkDoor :: SDoorState s -> Door s
mkDoor _ = MkDoor

toggleStateV3 :: forall s. SDoorStateI s => Door s -> SomeDoor 
toggleStateV3 = toS toggleStateV2

openAnyDoorV3 :: forall s. SDoorStateI s => Door s -> Door 'Open
openAnyDoorV3 = toS openAnyDoorV2

doorStateV3 :: forall s. SDoorStateI s => Door s -> DoorState
doorStateV3 = toS doorStateV2

doorState :: forall s. SDoorStateI s => Door s -> DoorState
doorState = doorStateV3
openAnyDoor :: forall s. SDoorStateI s => Door s -> Door 'Open
openAnyDoor = openAnyDoorV3
toggleState :: forall s. SDoorStateI s => Door s -> SomeDoor 
toggleState = toggleStateV3 

toS :: forall s r. SDoorStateI s => (SDoorState s -> r) -> r
toS f = f (sDoorState @s) 

class KnownDoorState s where
  doorStateVal :: Proxy s -> DoorState

-- with proxy
instance KnownDoorState 'Open where doorStateVal _ = Open

instance KnownDoorState 'Closed where doorStateVal _ = Closed

instance (KnownDoorState s) => KnownDoorState (Door s) where doorStateVal _ = doorStateVal (Proxy @s)
