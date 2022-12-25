{-# LANGUAGE TemplateHaskell #-}

module Chapter12.Base
  ( run,
  )
where

import Chapter12.ClassDerivation qualified
import Chapter12.Coerce qualified
import Chapter12.DerivingVia qualified
import Chapter12.Generics qualified
import Chapter12.GenericsSQL qualified
import Chapter12.Overlapping qualified
import Chapter12.TemplateValues qualified
import Utils

-- x = $(project 1 2) 1

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
        Chapter12.TemplateValues.run
    )
    "Chapter 12"
