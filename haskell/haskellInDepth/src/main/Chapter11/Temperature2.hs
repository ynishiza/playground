{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter11.Temperature2
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
    "Temperature2"
    ( do
        fmtLn $ nameF "freezing (C)" (build $ show freezing)
        fmtLn $ nameF "freezing (F)" (build $ show $ c2f freezing)
        fmtLn $ nameF "boiling (C)" (build $ show boiling)
        fmtLn $ nameF "boiling (F)" (build $ show $ c2f boiling)
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

c2f :: Temp C -> Temp F
c2f (Temp t) = Temp $ (t * 9 / 5) + 32

freezing :: Temp C
freezing = Temp @C 0

boiling :: Temp C
boiling = Temp @C 100
