{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter11.TypeFamilySynonyms
  ( run,
  )
where

import Data.Kind
import Fmt
import Utils

data TestValue
  = forall a.
    ( 
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
          let f :: (Show (Simplify a), CanSimplify a) => String -> a -> Builder
              f t s = "[" +| t |+ "] " +|| s ||+ "\t-simplify->\t" +|| simplify s ||+ ""
           in traverse (\(x, MkTestValue v) -> fmtLn $ f x v) testValues

        _ <-
          let f :: (Show (Widen a), CanWiden a) => String -> a -> Builder
              f t s = "[" +| t |+ "] " +|| s ||+ "\t-widen->\t" +|| widen s ||+ ""
           in traverse (\(x, MkTestValue v) -> fmtLn $ f x v) testValues
        pure ()
    )

type Simplify :: Type -> Type
type family Simplify (a :: Type)

type instance Simplify Int = Int

type instance Simplify Double = Int

type instance Simplify Bool = Int

type instance Simplify String = String

type instance Simplify Char = String

type CanSimplify :: Type -> Constraint
class Show a => CanSimplify (a :: Type) where
  simplify :: a -> Simplify a

instance CanSimplify Int where simplify = id

instance CanSimplify Double where simplify = round

instance CanSimplify Bool where simplify True = 1; simplify False = 0

instance CanSimplify Char where simplify = (: [])

instance CanSimplify String where simplify = id

type Widen :: Type -> Type
type family Widen (a :: Type) where
  Widen Double = Double
  Widen Int = Double
  Widen Char = String
  Widen a = String

type CanWiden :: Type -> Constraint
class Show a => CanWiden (a :: Type) where
  widen :: a -> Widen a

instance CanWiden Double where widen = id

instance CanWiden Int where widen = fromIntegral

instance CanWiden Bool where widen = show

instance CanWiden Char where widen = (: [])

instance CanWiden String where widen = id
