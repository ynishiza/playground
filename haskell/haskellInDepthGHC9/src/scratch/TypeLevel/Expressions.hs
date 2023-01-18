{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- ORMOLU_DISABLE -}
module TypeLevel.Expressions (
  test,
  BinaryOp(..),
  UnaryOp(..),
  IsExp(..),
  SNat(..),
  SUnaryOp(..),
  SBinaryOp(..),
  module X,
  ) where
{- ORMOLU_ENABLE -}

import Data.Kind
-- import Data.Singletons

import Data.Singletons.TH as X
import Data.Typeable
import Fmt as X
import GHC.TypeLits as X (KnownNat, Nat, natVal, sameNat)
import Utils

type E1 = 1

type E2 = 2

type E3 = (E1 ':+ E2)

type E4 = (E3 ':* E3)

type E5 = (E3 ':- E4)

type E6 = 'Neg 1
type E7 = 'Abs E6

test :: IO ()
test = do
  printEval @1
  printEval @2
  printEval @3
  printEval @(2 ':+ 3)
  printEval @(2 ':* 3)
  printEval @(2 ':- 3)
  printEval @(2 ':- 3)
  printEval @(1 ':+ (2 ':- (3 ':* 4)))

  printEval @E1
  printEval @E2
  printEval @E3
  printEval @E4
  printEval @E5
  printEval @E6
  printEval @E7
  pure ()

printEval :: forall {k} (a :: k). (SingI a, Typeable k, Typeable a, IsExp (Sing a) Integer) => IO ()
printEval = fmtLn $ showType (Proxy @a) |+ "\t=\t" +| (expEval (sing @a) :: Integer) |+ ""

type BinaryOp :: forall a b. a -> b -> Type
data BinaryOp a b where
  (:+) :: a -> b -> BinaryOp a b
  (:*) :: a -> b -> BinaryOp a b
  (:-) :: a -> b -> BinaryOp a b

type UnaryOp :: forall a. a -> Type
data UnaryOp a where
  Neg :: a -> UnaryOp a
  Abs :: a -> UnaryOp a

type IsExp :: Type -> Type -> Constraint
class IsExp a b where
  expEval :: a -> b

type SNat :: Nat -> Type
data SNat a where
  MkSNat :: (KnownNat a, SingI a) => SNat a

type SUnaryOp :: forall a. UnaryOp a -> Type
data SUnaryOp a where
  SNeg :: forall a. SingI a => SUnaryOp ('Neg a)
  SAbs :: forall a. SingI a => SUnaryOp ('Abs a)

type SBinaryOp :: forall a b. BinaryOp a b -> Type
data SBinaryOp a where
  SPlus :: forall a b. (SingI a, SingI b) => SBinaryOp (a ':+ b)
  SMult :: forall a b. (SingI a, SingI b) => SBinaryOp (a ':* b)
  SMinus :: forall a b. (SingI a, SingI b) => SBinaryOp (a ':- b)

type instance Sing = SNat

type instance Sing = SBinaryOp

type instance Sing = SUnaryOp

instance KnownNat a => SingI (a :: Nat) where sing = MkSNat

instance (SingI a, SingI b) => SingI (a ':+ b) where sing = SPlus

instance (SingI a, SingI b) => SingI (a ':* b) where sing = SMult

instance (SingI a, SingI b) => SingI (a ':- b) where sing = SMinus

instance SingI a => SingI ('Neg a) where sing = SNeg

instance SingI a => SingI ('Abs a) where sing = SAbs

instance SDecide a => SDecide (UnaryOp a) where
  a@SNeg %~ b@SNeg = unaryEq a (unaryGetArgs a) (unaryGetArgs b)
  a@SAbs %~ b@SAbs = unaryEq a (unaryGetArgs a) (unaryGetArgs b)
  _ %~ _ = Disproved $ \case {}

unaryGetArgs :: forall a p. (SingI a) => SUnaryOp (p a) -> Sing a
unaryGetArgs _ = sing @a

unaryEq :: forall k (a :: k) (b :: k) p. SDecide k => Sing (p a) -> Sing a -> Sing b -> Decision (p a :~: p b)
unaryEq _ a b = case a %~ b of
  Proved Refl -> Proved Refl
  Disproved rev -> Disproved $ \case Refl -> rev Refl

instance SDecide Nat where
  (MkSNat :: SNat a) %~ (MkSNat :: SNat b) = case sameNat (Proxy @a) (Proxy @b) of
    Just Refl -> Proved Refl
    Nothing -> Disproved $ error ""

instance (SDecide a, SDecide b) => SDecide (BinaryOp a b) where
  a@SPlus %~ b@SPlus = binaryEq a (binaryGetArgs a) (binaryGetArgs b)
  a@SMult %~ b@SMult = binaryEq a (binaryGetArgs a) (binaryGetArgs b)
  a@SMinus %~ b@SMinus = binaryEq a (binaryGetArgs a) (binaryGetArgs b)
  _ %~ _ = Disproved $ \case {}

binaryGetArgs :: forall a b p. (SingI a, SingI b) => SBinaryOp (p a b) -> (Sing a, Sing b)
binaryGetArgs _ = (sing @a, sing @b)

binaryEq :: forall k1 k2 (a :: k1) (b :: k2) (c :: k1) (d :: k2) op. (SDecide k1, SDecide k2) => Sing (op a b) -> (Sing a, Sing b) -> (Sing c, Sing d) -> Decision (op a b :~: op c d)
binaryEq _ (a, b) (c, d) = case a %~ c of
  Disproved rev -> Disproved $ \case Refl -> rev Refl
  Proved Refl -> case b %~ d of
    Proved Refl -> Proved Refl
    Disproved rev -> Disproved $ \case Refl -> rev Refl

instance KnownNat (a :: Nat) => IsExp (SNat a) Integer where
  expEval x@Sing = natVal x

instance (IsExp (Sing a) Integer, IsExp (Sing b) Integer) => IsExp (SBinaryOp (a ':+ b)) Integer where
  expEval SPlus = expEval (sing @a) + expEval (sing @b)

instance (IsExp (Sing a) Integer, IsExp (Sing b) Integer) => IsExp (SBinaryOp (a ':* b)) Integer where
  expEval SMult = expEval (sing @a) * expEval (sing @b)

instance (IsExp (Sing a) Integer, IsExp (Sing b) Integer) => IsExp (SBinaryOp (a ':- b)) Integer where
  expEval SMinus = expEval (sing @a) - expEval (sing @b)

instance IsExp (Sing a) Integer => IsExp (SUnaryOp ('Neg a)) Integer where
  expEval SNeg = negate $ expEval (sing @a)

instance IsExp (Sing a) Integer => IsExp (SUnaryOp ('Abs a)) Integer where
  expEval SAbs = abs $ expEval (sing @a)

data MyValue = A | B -- type=A kind=MyValue

-- step: define singletons
type SMyValue :: MyValue -> Type -- MUST be restricted to kind MyValue
data SMyValue a where
  SA :: SMyValue 'A -- singleton for ‘A
  SB :: SMyValue 'B

-- step:
type instance Sing = SMyValue

instance SingI 'A where
  sing :: SMyValue 'A
  sing = SA

instance SingI 'B where
  sing :: SMyValue 'B
  sing = SB
