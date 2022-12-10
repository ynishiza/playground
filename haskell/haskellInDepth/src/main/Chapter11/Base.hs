module Chapter11.Base
  ( run,
  )
where

import qualified Chapter11.PolymorphicKind
import qualified Chapter11.Temp
import qualified Chapter11.Temp2
import qualified Chapter11.TypeOperator
import Utils

run :: TestState
run =
  wrapTest
    ( do
        Chapter11.TypeOperator.run
        Chapter11.Temp.run
        Chapter11.Temp2.run
        Chapter11.PolymorphicKind.run
    )
    "Chapter 11"
