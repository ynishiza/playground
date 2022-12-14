module Chapter11.Base
  ( run,
  )
where

import qualified Chapter11.AssocFamilies
import qualified Chapter11.DataFamily
import qualified Chapter11.DataKinds
import qualified Chapter11.GADT
import qualified Chapter11.GADT2
import qualified Chapter11.PolyKinds
import qualified Chapter11.Temperature
import qualified Chapter11.Temperature2
import qualified Chapter11.TypeFamilySynonyms
import qualified Chapter11.TypeOperators
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
        Chapter11.TypeFamilySynonyms.run
        Chapter11.DataFamily.run
        Chapter11.AssocFamilies.run
        Chapter11.GADT.run
        Chapter11.GADT2.run
    )
    "Chapter 11"
