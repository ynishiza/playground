{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Problems
  ( mergedIsKnockable,
    test,
    appendHallways,
  )
where

-- import Data.Singletons.TH

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Kind
import Data.List.Singletons hiding (Map)
import Data.Typeable
import Door hiding (test)
import Fmt
import GHC.TypeLits as G
-- import GHC.TypeLits.Singletons
import Prelude.Singletons (NotSym0)
import Utils

-- ==================== Q1 ====================
type Knockable :: DoorState -> Type
data Knockable a where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

mergedIsKnockable :: Knockable a -> Knockable b -> Knockable (MergeState a b)
mergedIsKnockable KnockClosed KnockClosed = KnockClosed
mergedIsKnockable KnockClosed KnockLocked = KnockLocked
mergedIsKnockable KnockLocked KnockClosed = KnockLocked
mergedIsKnockable KnockLocked KnockLocked = KnockLocked

-- ==================== Q2 ====================

$( singletons
     [d|
       (+++) :: [a] -> [a] -> [a]
       [] +++ y = y
       (x : xs) +++ y = x : (xs +++ y)
       |]
 )

appendHallways :: forall s t. Hallway s -> Hallway t -> Hallway (s ++ t)
appendHallways s t = mkHallway $ withHall s $ withHall t $ (sing @s) %++ (sing @t)

appendHallways' :: forall s t. Hallway s -> Hallway t -> Hallway (s +++ t)
appendHallways' s t = mkHallway $ withHall s $ withHall t $ (sing @s) %+++ (sing @t)

withHall :: Hallway a -> (SingI a => r) -> r
withHall HEnd = id
withHall (MkDoor :<# ds) = withHall ds

mkHallway :: Sing s -> Hallway s
mkHallway SNil = HEnd
mkHallway (SCons x xs) = withSingI x MkDoor :<# mkHallway xs

mkHSomeHallway :: Sing (s :: [DoorState]) -> SomeHallway
mkHSomeHallway s = s :&: mkHallway s

appendSomeHallways :: SomeHallway -> SomeHallway -> SomeHallway
appendSomeHallways (s :&: h) (s' :&: h') = (s %+++ s') :&: (h `appendHallways'` h')

-- ==================== Q3 ====================
--
-- Step: setup
knock' :: forall s. Door s -> IO ()
knock' d@MkDoor = doorMessage d $ "Knocked door " ++ show (sing @s)

knock :: Knockable s -> Door s -> IO ()
knock _ = (putStr "[knock]" >>) . knock'

data Pass = Allow | Obstruct
  deriving (Eq, Show)

type StatePass :: DoorState -> Pass
type family StatePass (s :: DoorState) where
  StatePass 'Closed = 'Allow
  StatePass 'Locked = 'Allow
  StatePass 'Opened = 'Obstruct

knockP :: (StatePass s ~ 'Allow) => Door s -> IO ()
knockP = (putStr "[knockP]" >>) . knock'

-- Problem: Sigma with
type KnockableDoor :: DoorState ~> Type
data KnockableDoor a'

type instance Apply KnockableDoor a = (Knockable a, Door a)

type SomeKnockableDoor = Sigma DoorState KnockableDoor

knockSome :: SomeKnockableDoor -> IO ()
knockSome (_ :&: (k, d)) = putStr "[SomeKnockableDoor]" >> knock k d

-- Problem: Some with StatePass
type StatePassDoor :: DoorState ~> Type
data StatePassDoor a

type instance Apply StatePassDoor a = (StatePass a :~: 'Allow, Door a)

type SomeStatePassDoor = Sigma DoorState StatePassDoor

knockSomeStatePass :: SomeStatePassDoor -> IO ()
knockSomeStatePass (_ :&: (Refl, d)) = putStr "[SomeStatePassDoor]" >> knockP d

someKnockables :: [SomeKnockableDoor]
someKnockables =
  [ -- SOpened :&: (KnockClosed, MkDoor),    --  ERROR "Couldn't match type 'Closed with 'Opened"
    SClosed :&: (KnockClosed, MkDoor),
    SLocked :&: (KnockLocked, MkDoor)
  ]

someStatePassables :: [SomeStatePassDoor]
someStatePassables =
  [ -- SOpened :&: (Refl, MkDoor),     -- ERROR "Couldn't match type 'Allow with 'Obstruct"
    SClosed :&: (Refl, MkDoor),
    SLocked :&: (Refl, MkDoor)
  ]

-- ==================== Q4 ====================

type IsHalfOf :: Nat -> (Nat ~> Type)
data IsHalfOf a b'

type instance Apply (IsHalfOf n) m = (m G.* 2) :~: n

type IsEven n = Sigma Nat (IsHalfOf n)

mkIsEven :: forall (n :: Nat) (m :: Nat) p. (n ~ m G.* 2, KnownNat m) => p n -> p m -> IsEven n
mkIsEven _ _ = (sing @m) :&: Refl @n

showIsEven :: forall n. KnownNat n => IsEven n -> String
showIsEven s@(_ :&: Refl) = fmt $ natVal (Proxy @n) ||+ " = " +|| fstSigma s ||+ " * 2"

-- IsHalfMinusOneOf n m   n = m * 2 + 1
type IsHalfMinusOneOf :: Nat -> (Nat ~> Type)
data IsHalfMinusOneOf a b'

type instance Apply (IsHalfMinusOneOf n) m = (m G.* 2 + 1) :~: n

showIsOdd :: forall n. KnownNat n => IsOdd n -> String
showIsOdd s@(_ :&: Refl) = fmt $ natVal (Proxy @n) ||+ " = " +|| fstSigma s ||+ " * 2 + 1"

type IsOdd n = Sigma Nat (IsHalfMinusOneOf n)

type EvenProof :: Nat -> Type
data EvenProof n where
  EZ :: EvenProof 0
  ESS :: (SingI n, KnownNat n) => EvenProof n -> EvenProof (2 + n)

decideEven :: forall (n :: Nat). Sing n -> Decision (EvenProof n)
decideEven = undefined

-- ==================== Q5 ====================
-- Foldr :: (a ~> (b ~> b)) -> b -> [a] -> b
-- (.@#@$) :: (b ~> c) ~> ((a ~> b) ~> (a ~> c))
-- (.@#@$$) :: (b ~> c) -> (a ~> b) ~> (a ~> c)
type Map :: (a ~> b) -> [a] -> [b]
type Map f l = Door.Foldr (MapCons f) '[] l

type MapCons :: (a ~> b) -> (a ~> ([b] ~> [b]))
type MapCons f = (.@#@$) @@ TyCon2 (:) @@ f

-- type ConsSym1 :: a -> ([a] ~> [a])
-- data ConsSym1 a b

-- type instance Apply (ConsSym1 a) l = a ': l

-- type ConsSym0 :: a ~> ([a] ~> [a])
-- data ConsSym0 a

-- type instance Apply ConsSym0 a = ConsSym1 a

-- ==================== Q5 ====================
--
fromSomeDoors :: [SomeDoor] -> SomeHallway
fromSomeDoors [] = SNil :&: HEnd
fromSomeDoors ((dstate :&: _) : ds) = hall `appendSomeHallways` fromSomeDoors ds
  where
    hall = mkHSomeHallway (dstate `SCons` SNil)

assertTypeEqual :: (Typeable t1, Typeable t2) => t1 -> t2 -> IO ()
assertTypeEqual t1 t2 = when (s1 /= s2) $ throw (userError $ s1 ||+ " /= " +|| s2 ||+ "") 
  where
    s1 = typeOf t1
    s2 = typeOf t2

test :: IO ()
test = do
  printHeader "Q1"

  printHeader "Q2"
  printWithLabel ("append\t" +|| h1 ||+ "++" +|| h2 ||+ "\t") $ appendHallways h1 h2
  printWithLabel ("append\t" +|| h1 ||+ "++" +|| h3 ||+ "\t") $ appendHallways h1 h3
  printWithLabel ("append\t" +|| h2 ||+ "++" +|| h2 ||+ "\t") $ appendHallways h2 h2
  printWithLabel ("append\t" +|| h2 ||+ "++" +|| h3 ||+ "\t") $ appendHallways h2 h3
  let p = (,) <$> someHallways <*> someHallways
  (`traverse_` p) $ \(x, y) ->
    printWithLabel ("appendSomeHallways\t" +|| x ||+ " ++ " +|| y ||+ "\t") $ appendSomeHallways x y

  printHeader "Q3"
  (`traverse_` someKnockables) knockSome
  (`traverse_` someStatePassables) knockSomeStatePass

  printHeader "Q4"
  putStrLn $ showIsEven $ mkIsEven (sing @2) (sing @1)
  -- putStrLn $ showIsEven $ mkIsEven (sing @3) (sing @1)      -- ERROR. "Couldn't match type 3 with 2"
  putStrLn $ showIsEven $ mkIsEven (sing @4) (sing @2)
  putStrLn $ showIsEven $ mkIsEven (sing @6) (sing @3)
  putStrLn $ showIsEven $ mkIsEven (sing @10) (sing @5)
  putStrLn $ showIsOdd $ (sing @0) :&: (Refl @1)
  -- putStrLn $ showIsOdd $ (sing @0) :&: (Refl @2)    -- ERROR "Couldn't match type 2 with 1
  putStrLn $ showIsOdd $ (sing @1) :&: (Refl @3)
  putStrLn $ showIsOdd $ (sing @2) :&: (Refl @5)
  putStrLn $ showIsOdd $ (sing @3) :&: (Refl @7)

  printHeader "Q5"
  printWithLabel "Map NotSym0 '[ 'True, 'False]\t\t" $ showType (Proxy @(Map NotSym0 '[ 'True, 'False]))
  assertTypeEqual (Proxy @(Map NotSym0 '[ 'True, 'False])) (Proxy @['False, 'True])
  printWithLabel "Map (TyCon1 'Door) '[ 'Opened, 'Closed, 'Locked]\t\t" $ showType (Proxy @(Map (TyCon1 Door) '[ 'Opened, 'Closed, 'Locked]))
  assertTypeEqual (Proxy @(Map (TyCon1 Door) '[ 'Opened, 'Closed, 'Locked])) (Proxy @[Door 'Opened, Door 'Closed, Door 'Locked])
  printWithLabel "Map (TyCon1 'Just) '[ '1, 2, 3]\t\t" $ showType (Proxy @(Map (TyCon1 'Just) '[1, 2, 3]))
  assertTypeEqual (Proxy @(Map (TyCon1 'Just) '[1, 2, 3])) (Proxy @['Just 1, 'Just 2, 'Just 3])

  printHeader "Q6"
  printWithLabel "" $ fromSomeDoors []
  printWithLabel "" $ fromSomeDoors allSomeDoors
  printWithLabel "" $ fromSomeDoors $ take 1 allSomeDoors
  printWithLabel "" $ fromSomeDoors $ take 2 allSomeDoors
  printWithLabel "" $ fromSomeDoors $ reverse allSomeDoors
  printWithLabel "" $ fromSomeDoors $ take 10 $ cycle allSomeDoors
