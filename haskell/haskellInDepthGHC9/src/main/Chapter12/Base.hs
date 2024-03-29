{-# LANGUAGE TemplateHaskell #-}

module Chapter12.Base
  ( run,
    proj_1_0,
    proj_2_0,
    proj_2_1,
    proj_3_0,
    proj_3_1,
    proj_3_2,
    proj_4_0,
    proj_4_1,
    proj_4_2,
    proj_4_3,
    proj_5_0,
    proj_5_1,
    proj_5_2,
    proj_5_3,
    proj_5_4,
    proj_6_0,
    proj_6_1,
    proj_6_2,
    proj_6_3,
    proj_6_4,
    proj_6_5,
    proj_7_0,
    proj_7_1,
    proj_7_2,
    proj_7_3,
    proj_7_4,
    proj_7_5,
    proj_7_6,
    proj_8_0,
    proj_8_1,
    proj_8_2,
    proj_8_3,
    proj_8_4,
    proj_8_5,
    proj_8_6,
    proj_8_7,
    proj_9_0,
    proj_9_1,
    proj_9_2,
    proj_9_3,
    proj_9_4,
    proj_9_5,
    proj_9_6,
    proj_9_7,
    proj_9_8,
    proj_10_0,
    proj_10_1,
    proj_10_2,
    proj_10_3,
    proj_10_4,
    proj_10_5,
    proj_10_6,
    proj_10_7,
    proj_10_8,
    proj_10_9,
    listToTuple_1,
    listToTuple_2,
    listToTuple_3,
    listToTuple_4,
    listToTuple_5,
    listToTuple_6,
    listToTuple_7,
    listToTuple_8,
    listToTuple_9,
    listToTuple_10,
    someVeryRandomValue,
    someVeryRandomValue2,
    isCircle,
    isTriangle,
    isSquare,
    myVeryRandomValue,
    myVeryRandomFunction,
  )
where

import Chapter12.ClassDerivation qualified
import Chapter12.Coerce qualified
import Chapter12.DerivingVia qualified
import Chapter12.Generics qualified
import Chapter12.GenericsSQL qualified
import Chapter12.Overlapping qualified
import Chapter12.Template.ListToTuple
import Chapter12.Template.Projection
import Chapter12.Template.Reify
import Chapter12.Template.Values qualified
import Language.Haskell.TH
import Utils

data Shape = Circle | Triangle | Square 
  deriving (Show, Eq)

$(listToTupleDeclareMany 1 10)
$(projectionDeclareMany 1 10)
$(myRandomValueDeclare)
$(myRandomValueDeclare2)
$(genIsSimple (mkName "Shape"))

Chapter12.Template.Values.myVeryRandomValueDec
Chapter12.Template.Values.myVeryRandomFunctionDec

run :: TestState
run =
  wrapTest
    ( do
        Chapter12.ClassDerivation.run
        Chapter12.DerivingVia.run
        Chapter12.Coerce.run
        Chapter12.Overlapping.run
        Chapter12.Generics.run
        Chapter12.GenericsSQL.run
        Chapter12.Template.Values.run
    )
    "Chapter 12"
