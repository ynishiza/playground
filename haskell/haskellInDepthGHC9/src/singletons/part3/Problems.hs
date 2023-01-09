{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Problems
  ( test,
    And (..),
    Or (..),
    refuteKnockableOpened,
  )
where

import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.Typeable
import Data.Void
import Door hiding (test)
import Fmt
import Utils

isDoorStatable :: Sing s -> Decision (SDoorState s)
isDoorStatable = Proved

-- ==================== Q2 ====================
refutedRefuted :: forall a. SingI a => Refuted (Refuted (Knockable a)) -> Knockable a
refutedRefuted v = case isKnockable (sing @a) of
  Proved proof -> proof
  Disproved (void :: Refuted (Knockable a)) -> absurd $ v void

refuteKnockableOpened :: Refuted (Knockable 'Opened)
refuteKnockableOpened x = case x of {}

refuteRefuteOpened :: Refuted (Refuted (Knockable 'Opened))
refuteRefuteOpened = error "This should be impossible to write!!"

refuteRefuteClosed :: Refuted (Refuted (Knockable 'Closed))
refuteRefuteClosed f = f KnockClosed

refuteRefuteLocked :: Refuted (Refuted (Knockable 'Locked))
refuteRefuteLocked f = f KnockLocked

-- ==================== Q3 ====================
data And p q a where
  And :: p a -> q a -> And p q a

data Or p q a where
  OrLeft :: p a -> Or p q a
  OrRight :: q a -> Or p q a

instance (Typeable p, Typeable q, Typeable (a :: k), Typeable k) => Show (And p q a) where
  show v@(And _ _) = "And [" +| showType v |+ "]"

instance (Typeable p, Typeable q, Typeable (a :: k), Typeable k) => Show (Or p q a) where
  show v@(OrLeft _) = "OrLeft [" +| showType v |+ "]"
  show v@(OrRight _) = "OrRight [" +| showType v |+ "]"

type DecisionFunction p = forall a. Sing a -> Decision (p a)

decideAnd :: DecisionFunction p -> DecisionFunction q -> Sing a -> Decision (And p q a)
-- decideAnd :: (forall b. Sing b -> Decision (p b)) -> (forall b. Sing b -> Decision (q b)) -> Sing a -> Decision (And p q a)
decideAnd f g s = case f s of
  Proved pf1 -> case g s of
    Proved pf2 -> Proved $ And pf1 pf2
    Disproved rf2 -> Disproved $ \(And _ v) -> rf2 v
  Disproved rf1 -> Disproved $ \(And v _) -> rf1 v

decideOr :: DecisionFunction p -> DecisionFunction q -> Sing a -> Decision (Or p q a)
decideOr f g s = case f s of
  Proved pf1 -> Proved $ OrLeft pf1
  Disproved rf1 -> case g s of
    Proved pf2 -> Proved $ OrRight pf2
    Disproved rf2 -> Disproved $ \case
      (OrLeft v) -> rf1 v
      (OrRight v) -> rf2 v

knockableAndOpened :: forall s. SingI s => Refuted (And Knockable ((:~:) 'Opened) s)
knockableAndOpened (And x y) = case x of
  KnockLocked -> case y of {}
  KnockClosed -> case y of {}

knockableOrOpened :: forall s. SingI s => Or Knockable ((:~:) 'Opened) s
knockableOrOpened = case sing @s of
  SClosed -> OrLeft KnockClosed
  SLocked -> OrLeft KnockLocked
  SOpened -> OrRight Refl

-- ==================== Q5 ====================
knockedRefute :: forall s. SingI s => Knockable s -> Refuted ((:~:) 'Opened s)
knockedRefute KnockClosed = \case {}
knockedRefute KnockLocked = \case {}

refuteKnocked :: forall s. SingI s => Refuted ((:~:) 'Opened s) -> Knockable s
refuteKnocked f = case (sing @s) of
  SClosed -> KnockClosed
  SLocked -> KnockLocked
  SOpened -> absurd $ f Refl

-- Q5.
knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ = (putStr "[knockRefl]" >>) . knock'

knockSomeDoorRefl :: SomeDoor -> IO ()
knockSomeDoorRefl (MkSomeDoor (d@MkDoor :: Door s)) = case sStatePass (sing @s) of
  SObstruct -> doorMessage d "knockSomeDoorRefl call" >> knockRefl Refl d
  SAllow -> doorMessage d "knockSomeDoorRefl not allowed"

-- ==================== Q6 ====================
--
knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv = (putStr "[knockInv]" >>) . knock'

knockSomeDoorInv :: SomeDoor -> IO ()
knockSomeDoorInv (MkSomeDoor (d@MkDoor :: Door s)) = case sStatePass (sing @s) of
  SObstruct -> putStrLn "knockSomeDoorInv: call" >> knockInv d
  SAllow -> putStrLn "knockSomeDoorInv: not allowed"

-- ==================== Q7 ====================
--
$( singletons
     [d|
       class Cycle a where
         next :: a -> a
         prev :: a -> a
       |]
 )

instance PCycle Pass where
  type Next 'Allow = 'Obstruct
  type Next 'Obstruct = 'Allow
  type Prev 'Allow = 'Obstruct
  type Prev 'Obstruct = 'Allow

instance SCycle Pass where
  sNext SAllow = SObstruct
  sNext SObstruct = SAllow
  sPrev SAllow = SObstruct
  sPrev SObstruct = SAllow

test :: IO ()
test = do
  printHeader "predicate :~:"
  putStrLn $ showProof $ SOpened %~ SOpened
  putStrLn $ showProof $ SOpened %~ SClosed
  putStrLn $ showProof $ SOpened %~ SLocked
  putStrLn $ showProof $ SClosed %~ SOpened
  putStrLn $ showProof $ SClosed %~ SClosed
  putStrLn $ showProof $ SClosed %~ SLocked
  putStrLn $ showProof $ SLocked %~ SOpened
  putStrLn $ showProof $ SLocked %~ SClosed
  putStrLn $ showProof $ SLocked %~ SLocked

  printHeader "isKnockable"
  putStrLn $ showProof $ isKnockable SOpened
  putStrLn $ showProof $ isKnockable SClosed
  putStrLn $ showProof $ isKnockable SLocked

  printHeader "Q2"
  printHeader "refute refute"
  printWithLabel "refute refute (Knockable 'Closed)" $ refutedRefuted refuteRefuteClosed
  printWithLabel "refute refute (Knockable 'Locked)" $ refutedRefuted refuteRefuteLocked
  printWithLabel "Refuted (Knockable 'Opened)" $ showType refuteKnockableOpened
  printWithLabel "Refuted Refuted (Knockable 'Closed)" $ showType refuteRefuteClosed

  printHeader "Q3"
  printWithLabel "knockableOrOpened @'Opened" $ knockableOrOpened @'Opened
  printWithLabel "knockableOrOpened @'Closed" $ knockableOrOpened @'Closed
  printWithLabel "knockableOrOpened @'Locked" $ knockableOrOpened @'Locked
  putStrLn "AND"
  putStrLn $ showProof $ decideAnd isKnockable (SOpened %~) SOpened
  putStrLn $ showProof $ decideAnd isKnockable (SOpened %~) SClosed
  putStrLn $ showProof $ decideAnd isKnockable (SOpened %~) SLocked
  putStrLn $ showProof $ decideAnd isKnockable (SClosed %~) SOpened
  putStrLn $ showProof $ decideAnd isKnockable (SClosed %~) SClosed
  putStrLn $ showProof $ decideAnd isKnockable (SClosed %~) SLocked
  putStrLn $ showProof $ decideAnd isKnockable (SLocked %~) SOpened
  putStrLn $ showProof $ decideAnd isKnockable (SLocked %~) SClosed
  putStrLn $ showProof $ decideAnd isKnockable (SLocked %~) SLocked

  putStrLn "OR"
  putStrLn $ showProof $ decideOr isKnockable (SOpened %~) SOpened
  putStrLn $ showProof $ decideOr isKnockable (SOpened %~) SClosed
  putStrLn $ showProof $ decideOr isKnockable (SOpened %~) SLocked
  putStrLn $ showProof $ decideOr isKnockable (SClosed %~) SOpened
  putStrLn $ showProof $ decideOr isKnockable (SClosed %~) SClosed
  putStrLn $ showProof $ decideOr isKnockable (SClosed %~) SLocked
  putStrLn $ showProof $ decideOr isKnockable (SLocked %~) SOpened
  putStrLn $ showProof $ decideOr isKnockable (SLocked %~) SClosed
  putStrLn $ showProof $ decideOr isKnockable (SLocked %~) SLocked

  printHeader "Q4"
  printWithLabel "'Closed !~ 'Open -> knockable " $ refuteKnocked @'Closed (\case {})
  printWithLabel "'Locked !~ 'Open -> knockable " $ refuteKnocked @'Locked (\case {})

  printHeader "Q5"
  foldSomeDoors knockSomeDoorRefl

  printHeader "Q6"
  foldSomeDoors knockSomeDoorInv

  printHeader "Q7"
  printWithLabel "Next 'Allow" $ showType (Proxy @(Next 'Allow))
  printWithLabel "Prev 'Allow" $ showType (Proxy @(Prev 'Allow))
  printWithLabel "sNExt SAllow" $ show (sNext SAllow)
  printWithLabel "sPrev SAllow" $ show (sPrev SAllow)
