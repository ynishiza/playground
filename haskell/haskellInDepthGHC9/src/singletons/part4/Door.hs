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
    Hallway(..),
    traverseSomeDoors,
    foldSomeDoors,
    test,
    MergeStateList,
    -- MF2,
    -- MF,
  )
where
{- ORMOLU_ENABLE -}

import Data.Eq.Singletons
-- import Data.Foldable.Singletons
import Data.Kind
import Data.Ord.Singletons
import Data.Singletons
-- import Data.Singletons.Base.TH
import Data.Singletons.Base.TH hiding (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, PFoldable, sFoldr)
import Defunctionalization (FOLDR)
import Defunctionalization qualified as D
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
  show MkDoor = fmt $ "Door " +|| sing @s ||+ ""

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

type Hallway :: [DoorState] -> Type
data Hallway a where
  HEnd :: Hallway '[]
  (:<#) :: Door s -> Hallway ss -> Hallway (s ': ss)

infixr 2 :<#

instance Show (Hallway a) where
  show HEnd = "_"
  show (d :<# h) = fmt $ d ||+ " -> " +|| h ||+ ""

data SomeHallways where
  MkSomeHallways :: forall a. Hallway a -> SomeHallways

$( singletons
     [d|
       mergeStateList :: [DoorState] -> DoorState
       mergeStateList [] = Opened
       mergeStateList (s : ss) = s `mergeState` mergeStateList ss
       |]
 )

$( singletons
     [d|
       foldr :: (a -> b -> b) -> b -> [a] -> b
       foldr _ z [] = z
       foldr f z (x : xs) = f x (foldr f z xs)
       |]
 )

-- collapseHallway :: Hallway a -> Door (MergeStateList a)
collapseHallway :: Hallway a -> Door (MergeStateList a)
collapseHallway HEnd = MkDoor
collapseHallway (d :<# h) = mergeDoors d $ collapseHallway h

-- collapseHallwayWithFold :: Hallway s -> Door (MF s)
-- collapseHallwayWithFold HEnd = MkDoor
-- collapseHallwayWithFold (d :<# h) = d `mergeDoors` (collapseHallwayWithFold h)

collapseHallwayWithFold' :: Hallway s -> Door (FOLDR MERGESym0 'Opened s)
collapseHallwayWithFold' HEnd = MkDoor
collapseHallwayWithFold' (d :<# h) = d `mergeDoors` collapseHallwayWithFold' h

collapseHallway' ::
  Hallway ss ->
  Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
collapseHallway' HEnd = MkDoor
collapseHallway' (d :<# ds) = d `mergeDoors` collapseHallway' ds

type MERGESym0 :: DoorState D.~> (DoorState D.~> DoorState)
data MERGESym0 a'

type MERGESym1 :: DoorState -> (DoorState D.~> DoorState)
data MERGESym1 a b'

type instance D.APPLY MERGESym0 x = MERGESym1 x

type instance D.APPLY (MERGESym1 x) y = MergeState x y

-- data MG1 :: DoorState -> DoorState ~> DoorState

-- type instance Apply (MG1 s) t = MergeState s t

-- data MG2 :: DoorState ~> (DoorState ~> DoorState)

-- type instance Apply MG2 s = MG1 s

-- type MF :: [DoorState] -> DoorState
-- type MF x = Foldr MG2 'Opened x

-- type MF2 = FoldrSym2 MergeStateSym0 'Opened

-- type H1 = '[]

-- type H2 = ('Opened ': '[])

-- type H3 = ('Closed ': 'Opened ': '[])

-- type H4 = ('Opened ': 'Opened ': '[])

-- type H5 = ('Opened ': 'Locked ': 'Closed ': '[])

-- deq :: forall {k} (a :: k) (b :: k). (SDecide k, SingI a, SingI b) => Decision (a :~: b)
-- deq = (sing @a) %~ (sing @b)

-- e1 = deq @(MF H1) @(MergeStateList H1)

-- e2 = deq @(MF H2) @(MergeStateList H2)

-- e3 = deq @(MF H3) @(MergeStateList H3)
h1 :: Hallway '[]
h1 = HEnd
h2 :: Hallway '[ 'Opened]
h2 = openedDoor :<# HEnd
h3 :: Hallway '[ 'Opened, 'Closed, 'Locked]
h3 = openedDoor :<# closedDoor :<# lockedDoor :<# HEnd

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
  printWithLabel "Empty hallway" h1
  printWithLabel "Opened" h2
  printWithLabel "Opened -> Cloed -> Locked" h3

  printWithLabel "Collapse" $ collapseHallway h1
  printWithLabel "Collapse" $ collapseHallway h2
  printWithLabel "Collapse" $ collapseHallway' h1
  printWithLabel "Collapse" $ collapseHallway' h2

  -- printHeader "Equality"
  -- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H1)
  -- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H2)
  -- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H3)
  -- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H4)
  -- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H5)
  -- printWithLabel "" $ showProof $ deq @(MF H5) @(MF H5)
