{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
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
     test,
    Door (..),
    SomeDoor (..),
    DoorState (..),
    SDoorState (..),

    -- proposition with type constructor
    knock',
    knock,
    Knockable (..),
    isKnockable,
    disprovedOpened,

    -- proposition with type family
    Pass (..),
    getPass,
    SPass (..),
    StatePass,
    sStatePass,
    knockP,
    invertPass,
    InvertPass,

    traverseSomeDoors,
    foldSomeDoors,
    doorMessage,
  )
where
{- ORMOLU_ENABLE -}

import Data.Kind
import Data.Singletons
import Data.Singletons.Base.TH
import Data.Singletons.Decide
-- import Data.Eq.Singletons
-- import Data.Ord.Singletons
-- import Data.Singletons.Base.Enum
import Fmt
import Utils

$( singletons
     [d|
       data DoorState where
         Opened :: DoorState
         Closed :: DoorState
         Locked :: DoorState
         deriving (Eq, Ord, Show)
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

getDoorState :: SDoorState a -> DoorState
getDoorState SOpened = Opened
getDoorState SClosed = Closed
getDoorState SLocked = Locked

isOpened :: SomeDoor -> IO ()
isOpened (MkSomeDoor (_ :: Door s)) = case SOpened %~ (sing @s) of
  Proved _ -> fmtLn $ sing @s ||+ ": IsOpened"
  _ -> fmtLn $ sing @s ||+ ": NOT IsOpened"

traverseSomeDoors :: Applicative m => (SomeDoor -> m a) -> m [a]
traverseSomeDoors f = traverse f allSomeDoors

foldSomeDoors :: Monoid a => (SomeDoor -> a) -> a
foldSomeDoors f = foldMap f allSomeDoors

doorMessage :: forall s. Door s -> String -> IO ()
doorMessage MkDoor msg = fmtLn $ "[Door " +|| getDoorState (sing @s) ||+ "]" +| msg |+ ""

-------------------- START Predicate with type constructor --------------------
---

data Knockable (s :: DoorState) where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

deriving instance Eq (Knockable s)

deriving instance Show (Knockable s)

class Proved a where
  auto :: a

instance Proved (Knockable 'Closed) where auto = KnockClosed

instance Proved (Knockable 'Locked) where auto = KnockLocked

knock' :: forall s. Door s -> IO ()
knock' d@MkDoor = doorMessage d $ "Knocked door " ++ show (sing @s)

knock :: Knockable s -> Door s -> IO ()
knock _ = (putStr "[knock]" >>) . knock'

isKnockable :: forall s. Sing s -> Decision (Knockable s)
isKnockable SClosed = Proved KnockClosed
isKnockable SLocked = Proved KnockLocked
isKnockable _ = Disproved $ \v -> case v of {}

knockSomeDoor :: SomeDoor -> IO ()
knockSomeDoor (MkSomeDoor (d :: Door s)) = case isKnockable (sing @s) of
  Proved proof -> doorMessage d "knockSomeDoor: allowed" >> knock proof d
  _ -> doorMessage d "knockSomeDoor: Not allowed"

disprovedOpened :: Refuted (Knockable 'Opened)
disprovedOpened v = case v of {}

-------------------- END Predicate with type constructor --------------------

-------------------- START Predicate with type family --------------------
--
--
$( singletons
     [d|
       data Pass = Allow | Obstruct
        deriving (Eq, Show)

       invertPass :: Pass -> Pass
       invertPass Allow = Obstruct
       invertPass Obstruct = Allow
       |]
 )

getPass :: SPass s -> Pass
getPass SAllow = Allow
getPass SObstruct = Obstruct

type StatePass :: DoorState -> Pass
type family StatePass (s :: DoorState) where
  StatePass 'Closed = 'Allow
  StatePass 'Locked = 'Allow
  StatePass 'Opened = 'Obstruct

sStatePass :: forall s. SDoorState s -> Sing (StatePass s)
-- sStatePass x = sing @(StatePass s)        -- "No instance for SingI (StatePass s) arising from a use of sing". Why?
sStatePass SClosed = sing @(StatePass s)
sStatePass SLocked = sing @(StatePass s)
sStatePass SOpened = sing @(StatePass s)

knockP :: (StatePass s ~ 'Allow) => Door s -> IO ()
knockP = (putStr "[knockP]" >>) . knock'

knockSomeDoorP :: SomeDoor -> IO ()
knockSomeDoorP (MkSomeDoor (d :: Door s)) = case sStatePass (sing @s) of
  SAllow -> doorMessage d "knockSomeDoorP allowed" >> knockP d
  SObstruct -> doorMessage d "knockSomeDoorP not allowed"

-------------------- END Predicate with type familyype families --------------------

-------------------- START Predicate with type class --------------------
class IsKnockable (s :: DoorState)

instance IsKnockable 'Closed

instance IsKnockable 'Locked

knockC :: IsKnockable s => Door s -> IO ()
knockC = (putStr "[knockC]" >>) . knock'

knockSomeDoorC :: SomeDoor -> IO ()
knockSomeDoorC (MkSomeDoor (MkDoor :: Door s)) = error "How to derive IsKnockable s?"

-------------------- END Predicate with type class --------------------

-------------------- START Another predicate --------------------
type Openable :: DoorState -> Type
data Openable s where
  OpenableOpened :: Openable 'Opened
  OpenableClosed :: Openable 'Closed

isOpenable :: forall s. Sing s -> Decision (Openable s)
isOpenable s = case s of
  SOpened -> Proved OpenableOpened
  SClosed -> Proved OpenableClosed
  SLocked -> Disproved $ \case {}

openDoor :: forall s. Openable s -> Door s -> IO ()
openDoor OpenableOpened _ = fmtLn "Already open"
openDoor OpenableClosed _ = fmtLn "Opening closed door"

openSomeDoor :: SomeDoor -> IO ()
openSomeDoor (MkSomeDoor (d@MkDoor :: Door s)) = case isOpenable (sing @s) of
  Proved pf -> openDoor pf d
  Disproved _ -> fmtLn "Cannot open door"

-------------------- END Another predicate --------------------

test :: IO ()
test = do
  foldSomeDoors isOpened

  printHeader "knock: knockable predicate with Dec"
  knock KnockLocked lockedDoor
  knock KnockClosed closedDoor

  printHeader "knock with auto"
  -- knock auto openedDoor        -- "no instance for (Proved (Knockable 'Opened))"
  knock auto closedDoor
  knock auto lockedDoor

  printHeader "knockSomeDoor"
  foldSomeDoors knockSomeDoor

  -- let useKnock (MkSomeDoor d) = knock auto d  -- error "No instance for (Proved (Knockable s))"

  -- knockP openedDoor     -- error "couldn't match type 'Obstruct with 'Allow
  printHeader "knockP: knockable predicate with type family"
  knockP closedDoor
  knockP lockedDoor
  printHeader "knockSomeDoorP"
  foldSomeDoors knockSomeDoorP

  printHeader "knockC: knockable predicate with class"
  knockC closedDoor
  knockC lockedDoor

  printHeader "open door"
  foldSomeDoors openSomeDoor
  pure ()

-- x = knock $ MkDoor @'Opened
-- y = knock $ MkDoor @'Closed
