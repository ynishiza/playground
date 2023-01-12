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
{-# HLINT ignore "Use for_" #-}
{- ORMOLU_DISABLE -}
module Door
  ( 
    Door (..),
    SomeDoor (..),
    DoorState (..),
    SDoorState (..),
    traverseSomeDoors,
    foldSomeDoors,
    doorMessage, 
    getDoorState,

    MergeState,
    Hallway(..),
    SomeHallway(..),
    MergeStateList,

    openedDoor,
    closedDoor,
    lockedDoor,

    h1, h2, h3,
    allSomeDoors,
    someHallways,
    module X,
    test,

    Foldr,
    FoldrSym0,
    FoldrSym1,
    FoldrSym2,
  )
where
{- ORMOLU_ENABLE -}

import Data.Eq.Singletons
import Data.Foldable
-- import Data.Foldable.Singletons
import Data.Kind
import Data.Ord.Singletons
import Data.Singletons as X
-- import Data.Singletons.Base.TH
import Data.Singletons.Base.TH as X hiding (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, PFoldable, sFoldr, type (@@))
import Data.Singletons.Base.TH qualified as S
import Data.Singletons.Sigma as X
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
allSomeDoors = [SOpened :&: openedDoor, SClosed :&: closedDoor, SLocked :&: lockedDoor]

type SomeDoor :: Type
type SomeDoor = Sigma DoorState (TyCon Door)

instance Show (Door s) where
  show MkDoor = fmt $ "Door " +|| sing @s ||+ ""

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
mergeSomeDoors (s1 :&: d@MkDoor) (s2 :&: e@MkDoor) =
  withMeredDoorState d e $ sMergeState s1 s2 :&: mergeDoors d e

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

type SomeHallway = Sigma [DoorState] (TyCon Hallway)

-- data SomeHallway where
--   MkSomeHallways :: forall a. Hallway a -> SomeHallway

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

collapseHallway :: Hallway a -> Door (MergeStateList a)
collapseHallway HEnd = MkDoor
collapseHallway (d :<# h) = mergeDoors d $ collapseHallway h

collapseHallwayWithFold' :: Hallway s -> Door (FOLDR MERGESym0 'Opened s)
-- collapseHallwayWithFold' :: Hallway s -> Door (S.Foldr' MergeStateSym0 'Opened s)
-- collapseHallwayWithFold' :: Hallway s -> Door (S.Foldr MergeStateSym0 'Opened s)
collapseHallwayWithFold' HEnd = MkDoor
collapseHallwayWithFold' (d :<# h) = d `mergeDoors` collapseHallwayWithFold' h

collapseHallway' ::
  Hallway ss ->
  Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
collapseHallway' HEnd = MkDoor
collapseHallway' (d :<# ds) = d `mergeDoors` collapseHallway' ds

collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (s :&: h) = sMergeStateList s :&: collapseHallway h

collapseSomeHallway' :: SomeHallway -> SomeDoor
collapseSomeHallway' (s :&: h) = sFoldr (sing @MergeStateSym0) SOpened s :&: collapseHallway' h

type MERGESym0 :: DoorState D.~> (DoorState D.~> DoorState)
data MERGESym0 a'

type MERGESym1 :: DoorState -> (DoorState D.~> DoorState)
data MERGESym1 a b'

type instance D.APPLY MERGESym0 x = MERGESym1 x

type instance D.APPLY (MERGESym1 x) y = MergeState x y

h1 :: Hallway '[]
h1 = HEnd

h2 :: Hallway '[ 'Opened]
h2 = openedDoor :<# HEnd

h3 :: Hallway '[ 'Opened, 'Closed, 'Locked]
h3 = openedDoor :<# closedDoor :<# lockedDoor :<# HEnd

someHallways :: [SomeHallway]
someHallways =
  [ sing @'[] :&: h1,
    sing @'[ 'Opened] :&: h2,
    sing @'[ 'Opened, 'Closed, 'Locked] :&: h3
  ]

test :: IO ()
test = do
  foldSomeDoors
    ( \(_ :&: (d@MkDoor :: Door s)) -> do
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

  printHeader "collapseHallway"
  printWithLabel ("collapseHallway" +|| h1 ||+ "") $ collapseHallway h1
  printWithLabel ("collapseHallway" +|| h2 ||+ "") $ collapseHallway h2
  printWithLabel ("collapseHallway" +|| h3 ||+ "") $ collapseHallway h3

  printHeader "Fold symos"
  printWithLabel ("FoldrSym2: " +|| h2 ||+ "\t") (collapseHallway h3 :: Door (S.FoldrSym2 MergeStateSym0 'Opened @@ '[ 'Locked]))
  printWithLabel ("FoldrSym1: " +|| h2 ||+ "\t") (collapseHallway h3 :: Door (S.FoldrSym1 MergeStateSym0 @@ 'Opened @@ '[ 'Locked]))
  printWithLabel ("FoldrSym0: " +|| h2 ||+ "\t") (collapseHallway h3 :: Door (S.FoldrSym0 @@ MergeStateSym0 @@ 'Opened @@ '[ 'Locked]))

  printHeader ""
  flip traverse_ someHallways $ \s@(_ :&: h) -> do
    printWithLabel ("collapseHallway " +|| h ||+ ": \t") $ collapseHallway h
    printWithLabel ("collapseSomeHallway " +|| s ||+ ": \t") $ collapseSomeHallway s
    printWithLabel ("collapseSomeHallway' " +|| s ||+ ": \t") $ collapseSomeHallway' s

-- printHeader "Equality"
-- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H1)
-- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H2)
-- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H3)
-- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H4)
-- printWithLabel "" $ showProof $ deq @(MF H1) @(MF H5)
-- printWithLabel "" $ showProof $ deq @(MF H5) @(MF H5)
