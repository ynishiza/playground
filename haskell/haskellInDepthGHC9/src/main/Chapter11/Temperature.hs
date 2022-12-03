{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Chapter11.Temperature
  ( run,
    Temp (..),
    F,
    C,
    MyLabel (..),
    UnitName (..),
    module X,
  )
where

import Data.Proxy as X
import Fmt
import GHC.Exts (Constraint)
import Utils

testPhantom :: TestState
testPhantom =
  createTest
    ( do
        fmtLn $ nameF "freezing (C)" (build $ show freezing)
        fmtLn $ nameF "freezing (F)" (build $ show $ c2f freezing)
        fmtLn $ nameF "boiling (C)" (build $ show boiling)
        fmtLn $ nameF "boiling (F)" (build $ show $ c2f boiling)
        pure ()
    )
    "testPhantom"

newtype Temp u = Temp {getTemp :: Double}
  deriving (Eq)

instance UnitName u => Buildable (Temp u) where
  build = build . show

data F

data C

class MyLabel u where
  -- myLabel :: String
  myLabel :: Proxy u -> String

instance MyLabel (Temp u) where myLabel _ = "Temp"

instance MyLabel Int where myLabel _ = "Some Int"

type UnitName :: forall k. k -> Constraint
class UnitName u where
  unitName :: Proxy u -> String

instance UnitName F where unitName _ = "F"

instance UnitName C where unitName _ = "C"

instance forall u. UnitName u => UnitName (Temp u) where
  unitName _ = unitName (Proxy @u)

instance UnitName Temp where
  unitName _ = "Unspecified"

instance UnitName u => Show (Temp u) where
  show (Temp v) = show v ++ unitName (Proxy @(Temp u))

c2f :: Temp C -> Temp F
c2f (Temp t) = Temp $ (t * 9 / 5) + 32

freezing :: Temp C
freezing = Temp @C 0

boiling :: Temp C
boiling = Temp @C 100

run :: TestState
run =
  wrapTest
    ( do
        testPhantom
    )
    "11.1"
