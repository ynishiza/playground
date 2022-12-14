{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.TypeFamilySynonyms
  ( run,
    CanSimplify (..),
    CanWiden (..),
  )
where

import Data.Kind
import Data.Word
import Fmt
import Utils

data TestValue
  = forall a.
    ( Show a,
      CanSimplify a,
      Show (Simplify a),
      CanWiden a,
      Show (Widen a)
    ) =>
    MkTestValue a

run :: TestState
run =
  createChapterTest
    "11.3"
    "TypeFamilySynonyms"
    ( do
        printBanner "Simply"
        let testValues =
              [ ("Int", MkTestValue (1 :: Int)),
                ("Double", MkTestValue (1.1 :: Double)),
                ("Bool", MkTestValue True),
                ("Char", MkTestValue 'a'),
                ("String", MkTestValue ("Hello" :: String))
              ]
        _ <-
          let f :: (Show a, Show (Simplify a), CanSimplify a) => String -> a -> Builder
              f t s = "[" +| t |+ "] " +|| s ||+ "\t-simplify->\t" +|| simplify s ||+ ""
           in traverse (\(x, MkTestValue v) -> fmtLn $ f x v) testValues

        _ <-
          let f :: (Show a, Show (Widen a), CanWiden a) => String -> a -> Builder
              f t s = "[" +| t |+ "] " +|| s ||+ "\t-widen->\t" +|| widen s ||+ ""
           in traverse (\(x, MkTestValue v) -> fmtLn $ f x v) testValues
        pure ()
    )

-- type Simplify :: Type -> Type                 -- StandaloneKindSignatures
type family Simplify (a :: Type) :: Type

-- type family Simplify (a :: k) :: k

type instance Simplify Int = Word8

type instance Simplify Double = Word8

type instance Simplify Bool = Char

type instance Simplify String = String

type instance Simplify Char = String

type instance Simplify (Maybe a) = String

type family Simplify2 (a :: Type) :: Type where
  Simplify2 Int = Word8
  Simplify2 Double = Word8
  Simplify2 Bool = Char
  Simplify2 String = String
  Simplify2 Char = String
  forall a. Simplify2 (Maybe a) = String
-- Simplify2 (Maybe a) = String
  forall a. Simplify2 a = String

-- type CanSimplify :: Type -> Constraint        StandaloneKindSignatures
class CanSimplify (a :: Type) where
  simplify :: a -> Simplify a

instance CanSimplify Int where simplify = fromIntegral

instance CanSimplify Double where simplify = round

instance CanSimplify Bool where simplify True = 'T'; simplify False = 'F'

instance CanSimplify Char where simplify = (: [])

instance CanSimplify String where simplify = id

instance CanSimplify (Maybe a) where simplify _ = "Maybe"

-- type Widen :: forall a. a -> a        StandaloneKindSignatures
-- type Widen :: Type -> Type            StandaloneKindSignatures
type family Widen (a :: k) :: k where
  Widen Double = Double
  Widen Int = Double
  Widen Char = String
  forall a. Widen a = String

-- type CanWiden :: Type -> Constraint    StandaloneKindSignatures
class CanWiden (a :: Type) where
  widen :: a -> Widen a

instance CanWiden Double where widen = id

instance CanWiden Int where widen = fromIntegral

instance CanWiden Bool where widen = show

instance CanWiden Char where widen = (: [])

instance CanWiden String where widen = id
