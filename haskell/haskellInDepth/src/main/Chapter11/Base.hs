module Chapter11.Base
  ( run,
  )
where

import qualified Chapter11.PolyKinds
import qualified Chapter11.Temperature
import qualified Chapter11.Temperature2
import qualified Chapter11.TypeOperators
import qualified Chapter11.DataKinds
import Utils

run :: TestState
run =
  wrapTest
    ( do
        Chapter11.TypeOperators.run
        Chapter11.Temperature.run
        Chapter11.Temperature2.run
        Chapter11.PolyKinds.run
        Chapter11.DataKinds.run
    )
    "Chapter 11"
