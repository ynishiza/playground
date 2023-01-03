{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Door.DoorGen
  ( Door (..),
    SomeDoor (..),
    SDoorState (..),
    mkDoor,
    mkDoorFromState,
    mkDoorFromState2,
    mkDoorFromState3,
    parseDoor,
    openAnyDoor,
    toggleState,
    doorState,
    withSomeDoor,
    module Door.Common,
    module Data.Singletons,
    test,
  )
where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Door.Common (DoorState (..), SDoorState (..))

type Door :: DoorState -> Type
data Door s where
  MkDoor :: Door s
  deriving Show

type SomeDoor :: Type
data SomeDoor where
  MkSomeDoor :: forall (s :: DoorState). SingI s => Door s -> SomeDoor

instance {-# OVERLAPPING #-} Show (Door 'Open) where show _ = "Door 'Open"
instance {-# OVERLAPPING #-} Show (Door 'Closed) where show _ = "Door 'Closed"
instance Show (SDoorState 'Open) where show SOpen = "SOpen"

instance Show (SDoorState 'Closed) where show SClosed = "SClosed"

instance Show SomeDoor where
  show (MkSomeDoor d) = case doorState d of
                          Open -> show (MkDoor @'Open)
                          Closed -> show (MkDoor @'Closed)

type WithDoor a = forall s. SingI s => Door s -> a

withSomeDoor :: forall a. WithDoor a -> SomeDoor -> a
withSomeDoor f (MkSomeDoor d) = f d

openDoor :: Door 'Closed -> Door 'Open
openDoor _ = MkDoor

closeDoor :: Door 'Open -> Door 'Closed
closeDoor _ = MkDoor

doorState :: forall s. SingI s => Door s -> DoorState
doorState _ = fromSing (sing @s)

openAnyDoor :: forall s. SingI s => Door s -> Door 'Open
openAnyDoor d = case sing @s of
  SOpen -> openDoor $ closeDoor d
  SClosed -> openDoor d

toggleState :: forall s. SingI s => Door s -> SomeDoor
toggleState d = case sing @s of
  SOpen -> MkSomeDoor $ closeDoor d
  SClosed -> MkSomeDoor $ openDoor d

parseDoor :: String -> Maybe SomeDoor
parseDoor "Open" = Just $ MkSomeDoor (MkDoor @'Open)
parseDoor "Closed" = Just $ MkSomeDoor (MkDoor @'Closed)
parseDoor _ = Nothing

mkDoor :: SDoorState s -> Door s
mkDoor _ = MkDoor

mkDoorFromState :: DoorState -> SomeDoor
mkDoorFromState s = case toSing s of
  (SomeSing SOpen) -> MkSomeDoor $ mkDoor SOpen
  (SomeSing SClosed) -> MkSomeDoor $ mkDoor SClosed

mkDoorFromState2 :: DoorState -> SomeDoor
mkDoorFromState2 s =
  withSomeSing
    s
    ( \sg -> case sg of
        SOpen -> MkSomeDoor (mkDoor sg)
        SClosed -> MkSomeDoor (mkDoor sg)
    )

mkDoorFromState3 :: DoorState -> SomeDoor
mkDoorFromState3 Open = withSing @'Open (MkSomeDoor . mkDoor)
mkDoorFromState3 Closed = withSing @'Closed (MkSomeDoor . mkDoor)

printReturn :: Show b => (t -> b) -> t -> IO b
printReturn f x = print (f x) >> return (f x)

test :: IO ()
test = do
  putStrLn "==== from open door ===="
  _ <- printReturn openDoor MkDoor
    >>= printReturn closeDoor
    >>= printReturn openDoor 
    >>= printReturn closeDoor 
    >>= printReturn toggleState 
    >>= printReturn (withSomeDoor toggleState) 
    >>= printReturn (withSomeDoor toggleState) 
    >>= printReturn (withSomeDoor toggleState) 

  putStrLn "==== from closed door ===="
  _ <- printReturn closeDoor MkDoor
      >>= printReturn openDoor
      >>= printReturn closeDoor 
      >>= printReturn openDoor 
      >>= printReturn toggleState 
      >>= printReturn (withSomeDoor toggleState) 
      >>= printReturn (withSomeDoor toggleState) 
      >>= printReturn (withSomeDoor toggleState) 
  pure ()
