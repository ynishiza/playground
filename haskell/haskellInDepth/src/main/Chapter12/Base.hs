module Chapter12.Base (
  run
  ) where

import qualified Chapter12.ClassDerivation
import qualified Chapter12.DerivingVia
import qualified Chapter12.Coerce
import Utils

run :: TestState
run = wrapTest (do 
  Chapter12.ClassDerivation.run
  Chapter12.DerivingVia.run
  Chapter12.Coerce.run
  ) "Chapter 12"
