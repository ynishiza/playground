{-# LANGUAGE TemplateHaskell #-}
module Chapter12.Base (
  run
  ) where

import qualified Chapter12.ClassDerivation
import qualified Chapter12.DerivingVia
import qualified Chapter12.Coerce
import qualified Chapter12.Overlapping
import qualified Chapter12.Generics
import qualified Chapter12.GenericsSQL
import Chapter12.TemplateProjection
import Utils

-- x = $(project 1 2) 1

run :: TestState
run = wrapTest (do 
  Chapter12.ClassDerivation.run
  Chapter12.DerivingVia.run
  Chapter12.Coerce.run
  Chapter12.Overlapping.run
  Chapter12.Generics.run
  Chapter12.GenericsSQL.run
  ) "Chapter 12"
