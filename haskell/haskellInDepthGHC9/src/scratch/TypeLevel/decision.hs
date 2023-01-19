{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Type.Dec
import Data.Type.Equality
import Data.Void

main :: IO ()
main = test

data Nat where
  Z :: Nat
  SS :: Nat -> Nat

intIsBool :: Dec (Int :~: Bool)
intIsBool = No $ \case {}

intIsInt :: Dec (Int :~: Int)
intIsInt = Yes Refl

notIntIsBool :: Dec (Neg (Int :~: Bool))
notIntIsBool = Yes $ \case {}

intBool :: Neg (Int :~: Bool)
intBool x = case x of {}

intInt :: Neg (Neg (Int :~: Int))
intInt notEq = notEq Refl

data Not a where
  Not :: Neg a -> Not a

instance Decidable a => Decidable (Not a) where
  decide = case decide @a of
    No ng -> Yes $ Not ng
    Yes pf -> No $ \(Not ng) -> ng pf

data a :&&: b where
  (:&&:) :: a -> b -> a :&&: b
  deriving (Show)

data a :||: b where
  OL :: a -> a :||: b
  OR :: b -> a :||: b
  deriving (Show)

instance (Decidable a, Decidable b) => Decidable (a :&&: b) where
  decide = case decide @a of
    Yes a -> case decide @b of
      Yes b -> Yes $ a :&&: b
      No noB -> No $ \(_ :&&: b) -> noB b
    No noA -> No $ \(a :&&: _) -> noA a

instance (Decidable a, Decidable b) => Decidable (a :||: b) where
  decide = case decide @a of
    Yes a -> Yes $ OL a
    No noA -> case decide @b of
      Yes b -> Yes $ OR b
      No noB -> No $ \case
        (OL a) -> noA a
        (OR b) -> noB b

any :: Void -> b
any x = case x of {}

data A = A deriving (Show, Eq)

instance Decidable A where decide = Yes A

test :: IO ()
test = do
  putStrLn $ decShow $ decide @Void
  putStrLn $ decShow $ decide @(A :&&: A)
  putStrLn $ decShow $ decide @(A :&&: Void)
  putStrLn $ decShow $ decide @(Void :&&: A)
  putStrLn $ decShow $ decide @(A :||: A)
  putStrLn $ decShow $ decide @(A :||: Void)
  putStrLn $ decShow $ decide @(Void :||: A)
  let x2c :: Dec (A :||: Void)
      x2c = Yes $ OL A

  pure ()
