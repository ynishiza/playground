{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Chapter12.Coerce
  ( unwrap,
    wrap,
    unsafeWrap,
    run,
  )
where

import Data.Coerce
import Data.Kind
import Unsafe.Coerce
import Utils

run :: TestState
run =
  createChapterTest
    "12"
    "Coerce"
    ( do
        let v = 1 :: Int
         in do
              assertIsEqual (Wrap1 (Wrap1 (Wrap1 v))) (wrap v)
              -- assertIsEqual (Wrap1 (Wrap1 (Wrap1 v))) (unsafeWrap v)
              assertIsEqual (unwrap $ Wrap1 (Wrap1 (Wrap1 v))) v
              assertIsEqual (wrapPerson $ Person "Yui" v) (Person (Wrap1 "Yui") (Wrap1 v))
              assertIsEqual (unsafeWrap (UnsafeDummy (Wrap1 1))) (UnsafeDummy (Wrap1 $ Wrap1 v))
        testDone
    )

type DummyFamily :: forall k. k -> Type
type family DummyFamily a where
  DummyFamily a = DummyWrapper a

newtype DummyWrapper a = Wrap1 a deriving (Show, Eq)

unwrap :: DummyWrapper (DummyWrapper (DummyWrapper a)) -> a
unwrap = coerce

wrap :: a -> DummyWrapper (DummyWrapper (DummyWrapper a))
wrap = coerce

data Person n a = Person {name :: !n, age :: !a} deriving (Show, Eq)

wrapPerson :: Person n a -> Person (DummyWrapper n) (DummyWrapper a)
wrapPerson = coerce

data UnsafeDummy a = UnsafeDummy (DummyFamily a)

deriving instance Eq a => Eq (UnsafeDummy a)

deriving instance Show a => Show (UnsafeDummy a)

unsafeWrap :: UnsafeDummy Int -> UnsafeDummy (DummyWrapper Int)
-- unsafeWrap = coerce     -- ERROR error="Couldn't match type ‘Int’ with ‘DummyWrapper Int’ arising from a use of ‘coerce’"
unsafeWrap = unsafeCoerce
