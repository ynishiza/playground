{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoStarIsType #-}

module Chapter11.DataKinds (run, Temp(..), TUnit(..)) where

import Data.Proxy
import Fmt
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import Utils

run :: TestState
run =
  createChapterTest
    "11.2"
    ""
    ( do
        printBanner "Tempeature"
        fmtLn $ nameF "freezing (C)" (build $ show freezing)
        fmtLn $ nameF "freezing (F)" (build $ show $ c2f freezing)
        fmtLn $ nameF "boiling (C)" (build $ show boiling)
        fmtLn $ nameF "boiling (F)" (build $ show $ c2f boiling)

        printBanner "Pointer"
        let p1 = Pointer @1 10
            p1v = toPointerValue p1
            p2 = Pointer @2 10
            p2v = toPointerValue p2
         in do
              fmtLn $ nameF "p1" (showBuilder p1)
              fmtLn $ nameF "p2" (showBuilder p2)
              fmtLn $ nameF "p1" $ p1 ||+ "->" +| p1v |+ "->" +|| fromPointerValue @1 p1v ||+ ""
              fmtLn $ nameF "p2" $ p2 ||+ "->" +| p2v |+ "->" +|| fromPointerValue @2 p2v ||+ ""
              fmtLn $ nameF "11 to Pointer @2" $ (11 :: Int) |+ "->" +|| fromPointerValue @2 11 ||+ ""
              fmtLn $ nameF "10 to Pointer @2" $ (10 :: Int) |+ "->" +|| fromPointerValue @2 10 ||+ ""

        printBanner "SurroundedString"
        print $ roundBrackets "hello"
        print $ squareBrackets "hello"
        print (MkSurroundedString "hello" :: SurroundedString "(" ")")
        print (MkSurroundedString "hello" :: SurroundedString "[" "]")

        printBanner "Fixed"
        print (MkFixedPoint 1234 :: FixedPoint 1)
        print (MkFixedPoint 1234 :: FixedPoint 10)
        print (MkFixedPoint 1234 :: FixedPoint 100)
        print C
        assertIsEqual freezingF (c2f freezing)
        testDone
    )

data TUnit = C | F | K deriving (Show)

class UnitName (u :: TUnit) where
  unitName :: String

instance UnitName 'C where
  unitName = show C

instance UnitName 'F where
  unitName = show F

newtype Temp (c :: TUnit) = MkTemp Double
  deriving (Num, Fractional, Eq)

freezing :: Temp 'C
freezing = MkTemp @'C 0

freezingF :: Temp 'F
freezingF = MkTemp @'F 32

boiling :: Temp 'C
boiling = MkTemp @'C 100

instance UnitName u => Show (Temp u) where show (MkTemp v) = v ||+ "" +| unitName @u |+ ""

c2f :: Temp 'C -> Temp 'F
c2f (MkTemp t) = MkTemp $ (t * 9 / 5) + 32

newtype Pointer (a :: Nat) = Pointer Integer

instance KnownNat a => Show (Pointer a) where
  show p@(Pointer v) = "Pointer " +| natVal p |+ " * " +| v |+ ""

toPointerValue :: KnownNat a => Pointer a -> Integer
toPointerValue p@(Pointer v) = natVal p * v

fromPointerValue :: forall a. KnownNat a => Integer -> Maybe (Pointer a)
fromPointerValue v = case r of
  0 -> Just (Pointer v')
  _ -> Nothing
  where
    (v', r) = divMod v $ natVal (Proxy @a)

newtype SurroundedString (p :: Symbol) (s :: Symbol) = MkSurroundedString String

roundBrackets :: String -> SurroundedString "(" ")"
roundBrackets = MkSurroundedString @"(" @")"

squareBrackets :: String -> SurroundedString "[" "]"
squareBrackets = MkSurroundedString @"[" @"]"

instance (KnownSymbol p, KnownSymbol s) => Show (SurroundedString p s) where
  show (MkSurroundedString v) = symbolVal (Proxy @p) ++ v ++ symbolVal (Proxy @s)

newtype FixedPoint (f :: Nat) = MkFixedPoint Integer

instance KnownNat f => Show (FixedPoint f) where
  show = show . (getValue)

getValue :: (KnownNat f, Fractional b) => FixedPoint f -> b
getValue p@(MkFixedPoint v) = fromInteger v / fromInteger (natVal p)
