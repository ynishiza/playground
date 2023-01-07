{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Elevator.Move
  (
  Move(..),
  decideMove,
  )
where

import Data.Type.Dec
import Data.Void
import Data.Type.Nat
import Data.Type.Nat.LE
import Elevator.Floor

type Move :: Nat -> Nat -> Nat -> Type
data Move mx to from where
  StandStill :: to ~ from => Move mx to from
  GoingUp :: BelowTop mx from => Move mx to from
  GoingDown :: (from ~ 'S f) => Move mx to from

decideMove :: forall mx to from. Floor mx to -> Floor mx from -> Move mx to from
decideMove MkFloor MkFloor
  -- case : to == from
  | Yes pf <- eq = gcastWith pf StandStill
  -- case: 0 < to < from
  | Yes (LESucc _) <- leq = GoingDown
  -- case: 0 == to < from
  | No n <- eq, Yes LEZero <- leq = case snat :: SNat from of
                     SS -> GoingDown
                     SZ -> absurd $ n Refl
  -- case: from < to
  | No p <- leq =
      let l = leProof @to @mx
          x = leTrans (leSwap p) l
       in withLEProof x GoingUp
  where
    eq = discreteNat @to @from
    leq = decideLE @to @from
