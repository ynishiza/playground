{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Chapter11.Temp
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

f2c :: Temp F -> Temp C
f2c (Temp v) = Temp $ (v - 32) * 5 / 9

run :: TestState
run =
  wrapTest
    ( do
        testPhantom
    )
    "11.1"

testPhantom :: TestState
testPhantom =
  createTest
    ( do
        let f1 = Temp @F 10
            f2 = Temp @F 10
        fmtLn $ "f:" +| f1 |+ " c:" +| f2c f1 |+ ""
        fmtLn $ "f:" +| f2 |+ " c:" +| f2c f2 |+ ""

        pure ()
    )
    "testPhantom"
