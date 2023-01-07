{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Door.DoorGen
  ( Door (..),
    SomeDoor (..),
    SDoorState (..),
    fromDoorState,
    fromSDoorState,
    fromSDoorStateI,
    openDoor,
    closeDoor,
    parseDoor,
    openAnyDoor,
    closeAnyDoor,
    toggleState,
    doorState,
    withSomeDoor,
    withSomeDoorI,
    module Door.Common,
    module Data.Singletons,
    test,
  )
where

import Data.Kind
import Data.Singletons
import Door.Common (DoorState (..), SDoorState (..))
import Fmt

type Door :: DoorState -> Type
data Door s where
  MkDoor :: Door s
  deriving (Show)

type SomeDoor :: Type
data SomeDoor where
  MkSomeDoor :: forall (s :: DoorState). SingI s => Door s -> SomeDoor

instance {-# OVERLAPPING #-} Show (Door 'Opened) where show _ = "Door 'Opened"

instance {-# OVERLAPPING #-} Show (Door 'Closed) where show _ = "Door 'Closed"

instance Show (SDoorState 'Opened) where show SOpened = "SOpened"

instance Show (SDoorState 'Closed) where show SClosed = "SClosed"

instance Show SomeDoor where
  show (MkSomeDoor d) = case doorState d of
    Opened -> "SomeDoor" +|| (MkDoor @'Opened) ||+ ""
    Closed -> "SomeDoor" +|| (MkDoor @'Closed) ||+ ""

instance SingI s => Eq (Door s) where
  x == y = doorState x == doorState y

instance Eq SomeDoor where
  (MkSomeDoor x) == (MkSomeDoor y) = doorState x == doorState y

type WithDoor a = forall s. SingI s => Door s -> a

fromDoorState :: DoorState -> SomeDoor
fromDoorState d = case toSing d of
  (SomeSing SOpened) -> MkSomeDoor $ fromSDoorState SOpened
  (SomeSing SClosed) -> MkSomeDoor $ fromSDoorState SClosed

-- fromDoorState Opened = MkSomeDoor $ fromSDoorState SOpened
-- fromDoorState Closed = MkSomeDoor $ fromSDoorState SClosed

fromSDoorState :: SDoorState s -> Door s
fromSDoorState _ = MkDoor

fromSDoorStateI :: forall s. SingI (s :: DoorState) => Door s
fromSDoorStateI = withSing @s fromSDoorState

withSomeDoor :: (forall s. SDoorState s -> Door s -> a) -> SomeDoor -> a
withSomeDoor f (MkSomeDoor d) = withSing (flip f d)

withSomeDoorI :: forall a. (forall s. SingI s => Door s -> a) -> SomeDoor -> a
withSomeDoorI f (MkSomeDoor d) = f d

(#>) :: SomeDoor -> WithDoor a -> a
d #> f = withSomeDoorI f d

infixl 1 #>

openDoor :: Door 'Closed -> Door 'Opened
openDoor _ = MkDoor

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor _ = MkDoor

doorState :: forall s. SingI s => Door s -> DoorState
doorState _ = fromSing (sing @s)

openAnyDoor' :: SDoorState s -> Door s -> Door 'Opened
openAnyDoor' SOpened = id
openAnyDoor' SClosed = openDoor

openAnyDoor :: forall s. SingI s => Door s -> Door 'Opened
openAnyDoor = withSing openAnyDoor'

closeAnyDoor' :: SDoorState s -> Door s -> Door 'Closed
closeAnyDoor' SClosed = id
closeAnyDoor' SOpened = closeDoor

closeAnyDoor :: forall s. SingI s => Door s -> Door 'Closed
closeAnyDoor = withSing closeAnyDoor'

toggleState' :: SDoorState s -> Door s -> SomeDoor
toggleState' SOpened = MkSomeDoor . closeDoor
toggleState' SClosed = MkSomeDoor . openDoor

toggleState :: forall s. SingI s => Door s -> SomeDoor
toggleState = withSing toggleState'

parseDoor :: String -> Maybe SomeDoor
parseDoor "Opened" = Just $ MkSomeDoor (MkDoor @'Opened)
parseDoor "Closed" = Just $ MkSomeDoor (MkDoor @'Closed)
parseDoor _ = Nothing

printReturn :: (Show a, Show b) => (a -> b) -> a -> IO b
printReturn f x = fmtLn (x ||+ " -> " +|| f x ||+ "") >> return (f x)

test :: IO ()
test = do
  putStrLn "==== from open door ===="
  _ <-
    printReturn openDoor MkDoor
      >>= printReturn closeDoor
      >>= printReturn openDoor
      >>= printReturn closeDoor
      >>= printReturn toggleState
      >>= printReturn (withSomeDoorI toggleState)
      >>= printReturn (withSomeDoorI toggleState)
      >>= printReturn (withSomeDoorI toggleState)

  putStrLn "==== from closed door ===="
  _ <-
    printReturn closeDoor MkDoor
      >>= printReturn openDoor
      >>= printReturn closeDoor
      >>= printReturn openDoor
      >>= printReturn toggleState
      >>= printReturn (withSomeDoorI toggleState)
      >>= printReturn (withSomeDoorI toggleState)
      >>= printReturn (withSomeDoorI toggleState)
      >>= printReturn (\a -> a #> toggleState #> toggleState #> toggleState #> toggleState)
      >>= printReturn (\a -> a #> toggleState #> toggleState #> toggleState)

  pure ()

type T2 :: forall {a}. a -> Type
data T2 a = T2
-- x = T2 @'False
f :: forall {k}  (a::k). Proxy a
f=Proxy
x=f @Maybe
