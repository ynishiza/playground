{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter11.Temp2
  ( run,
    Temp (..),
    UnitName (..),
  )
where

import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "11.1.3"
    ""
    ( do
        let x = Temp @C 1
            y = Temp @F 1
        fmtLn $ nameF "x" (build x) <> nameF "y" (build y)
        pure ()
    )

newtype Temp u = Temp Double

data C

data F

class UnitName u where
  unitName :: String

instance UnitName C where
  unitName = "C"

instance UnitName F where
  unitName = "F"

instance UnitName u => UnitName (Temp u) where
  unitName = unitName @u

instance UnitName u => Show (Temp u) where
  show (Temp v) = v ||+ "" +| unitName @u |+ ""

instance UnitName u => Buildable (Temp u) where
  build = build . show
