{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter12.ClassDerivation
  ( Age,
    run,
    fromAge,
    toAge,
  )
where

import Data.Aeson
import Data.Coerce
import Data.Typeable
import Fmt
import GHC.Generics (Generic)
import Unsafe.Coerce
import Utils

run :: TestState
run = createChapterTest "12.1.1" "Derivation" $ do
  let a = 14
      v = Age a

      n = ""
   in printBannerWrap "Test Age" $ do
        fmtLn $
          nameF "v+v" (build $ v + v)
            <> nameF "json" (showBuilder $ encode v)
        assertIsEqual (Age 1 + Age 2) (Age 3)
        assertIsEqual "14" (encode v)
        assertIsEqual (fromAge $ Age 140) 140
        assertIsEqual (toAge 140) (Age 140)
        assertIsEqual (toAge 140) (Age 140)
        assertIsEqual (coerceStudentAge $ Student n a) (Student n (Age a))
        assertIsEqual (coerceStudentMaybeAge $ StudentMaybe n (Just a)) (StudentMaybe n (Just $ Age a))
        assertIsEqual (coerceStudentIdFamilyAge $ StudentIdFamily n a) (StudentIdFamily n (Age a))
        pure ()
  testDone

class MyClass a where
  myId :: a -> a
  myId = id

newtype Age = Age Int
  deriving stock (Eq, Show, Typeable, Generic, Bounded)
  deriving newtype (Num, Buildable)
  deriving anyclass (ToJSON, MyClass)

type family Id a where
  Id t = t

data Student a = Student !String !a
  deriving stock (Eq, Show, Generic)

data StudentM m a = StudentM !String !(m a) deriving stock (Eq, Show, Generic)

data StudentMaybe a = StudentMaybe !String !(Maybe a) deriving stock (Eq, Show, Generic)

data StudentIdFamily a = StudentIdFamily !String !(Id a) deriving stock (Eq, Show, Generic)

toAge :: Int -> Age
toAge = coerce

fromAge :: Age -> Int
fromAge = coerce

coerceStudentAge :: Student Int -> Student Age
coerceStudentAge = coerce

coerceStudentMaybeAge :: StudentMaybe Int -> StudentMaybe Age
coerceStudentMaybeAge = coerce

-- coerceStudentMAge :: StudentM m Int -> StudentM m Age
-- coerceStudentMAge = coerce    -- ERROR "Couldn't match type Int with Age"

coerceStudentIdFamilyAge :: StudentIdFamily Int -> StudentIdFamily Age
coerceStudentIdFamilyAge = unsafeCoerce

-- coerceStudentIdFamilyAge = coerce      -- ERROR "Couldn't match type Int with Age"
