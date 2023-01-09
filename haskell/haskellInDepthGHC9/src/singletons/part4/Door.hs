{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{- ORMOLU_DISABLE -}
module Door
  ( 
    Door (..),
    SomeDoor (..),
    DoorState (..),
    SDoorState (..),
    traverseSomeDoors,
    foldSomeDoors,
    test,
  )
where
{- ORMOLU_ENABLE -}

import Data.Eq.Singletons
import Data.Kind
import Data.Ord.Singletons
import Data.Singletons
import Data.Ord.Singletons
import Data.Singletons.Base.Enum
-- import Data.Singletons.TH qualified as S
import Data.Singletons.Base.TH
import Fmt
import Utils

$( singletons
     [d|
       type DoorState :: Type
       data DoorState where
         Opened :: DoorState
         Closed :: DoorState
         Locked :: DoorState
         deriving (Eq, Ord, Show)
       |]
 )

$( singletons
     [d|
       mergeState :: DoorState -> DoorState -> DoorState
       mergeState = max
       |]
 )

type Door :: DoorState -> Type
data Door s where
  MkDoor :: SingI s => Door s

openedDoor :: Door 'Opened
openedDoor = MkDoor @'Opened

closedDoor :: Door 'Closed
closedDoor = MkDoor @'Closed

lockedDoor :: Door 'Locked
lockedDoor = MkDoor @'Locked

allSomeDoors :: [SomeDoor]
allSomeDoors = [MkSomeDoor openedDoor, MkSomeDoor closedDoor, MkSomeDoor lockedDoor]

type SomeDoor :: Type
data SomeDoor where
  MkSomeDoor :: forall (s :: DoorState). SingI s => Door s -> SomeDoor

instance Show (Door s) where
  show MkDoor = fmtLn $ "Door " +|| sing @s ||+ ""

instance Show SomeDoor where
  show (MkSomeDoor d@MkDoor) = fmtLn $ "SomeDoor " +|| d ||+ ""

getDoorState :: SDoorState a -> DoorState
getDoorState SOpened = Opened
getDoorState SClosed = Closed
getDoorState SLocked = Locked

traverseSomeDoors :: Applicative m => (SomeDoor -> m a) -> m [a]
traverseSomeDoors f = traverse f allSomeDoors

foldSomeDoors :: Monoid a => (SomeDoor -> a) -> a
foldSomeDoors f = foldMap f allSomeDoors

doorMessage :: forall s. Door s -> String -> IO ()
doorMessage MkDoor msg = fmtLn $ "[Door " +|| getDoorState (sing @s) ||+ "]" +| msg |+ ""

-- ==================== Part 4 main ====================

mergeDoors :: forall s t. Door s -> Door t -> Door (MergeState s t)
mergeDoors d@MkDoor e@MkDoor = withMeredDoorState d e MkDoor

mergeSomeDoors :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoors (MkSomeDoor d@MkDoor) (MkSomeDoor e@MkDoor) =
  withMeredDoorState d e $ MkSomeDoor $ mergeDoors d e

withMeredDoorState :: forall s t r. Door s -> Door t -> (SingI (MergeState s t) => r) -> r
withMeredDoorState MkDoor MkDoor = withMergedState (sing @s) (sing @t)

withMergedState :: Sing s -> Sing t -> (SingI (MergeState s t) => r) -> r
withMergedState SOpened SOpened f = f
withMergedState SOpened SClosed f = f
withMergedState SOpened SLocked f = f
withMergedState SClosed SOpened f = f
withMergedState SClosed SClosed f = f
withMergedState SClosed SLocked f = f
withMergedState SLocked SOpened f = f
withMergedState SLocked SClosed f = f
withMergedState SLocked SLocked f = f

data Hallway a where
  HEnd :: Hallway '[]
  (:<#) :: Door s -> Hallway ss -> Hallway (s ': ss)
infixr 2 :<#

$(singletons [d|
  mergeStateList :: [DoorState] -> DoorState
  mergeStateList [] = Opened
  mergeStateList (s:ss) = mergeState s (mergeStateList ss)
  |])

test :: IO ()
test = do
  foldSomeDoors
    ( \(MkSomeDoor (d@MkDoor :: Door s)) -> do
        let st = show $ getDoorState $ sing @s
        printWithLabel ("mergeDoors: " <> st <> " Opened") $ show $ mergeDoors d openedDoor
        printWithLabel ("mergeDoors: " <> st <> " Closed") $ show $ mergeDoors d closedDoor
        printWithLabel ("mergeDoors: " <> st <> " Locked") $ show $ mergeDoors d lockedDoor
        pure ()
    )

  printHeader "Hallway"
  printWithLabel "Empty hallway" $ showType HEnd
  printWithLabel "Opened" $ showType $ openedDoor :<# HEnd
  printWithLabel "Opened -> Cloed -> Locked" $ showType $ openedDoor :<# closedDoor :<# lockedDoor :<# HEnd
