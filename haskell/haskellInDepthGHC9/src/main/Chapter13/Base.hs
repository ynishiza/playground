module Chapter13.Base
  ( run,
  )
where

import Chapter13.MyNat qualified
import Chapter13.SingletonBasics qualified
import Chapter13.TypePatternMatching qualified
import Utils

run :: TestState
run =
  wrapTest
    ( do
        Chapter13.MyNat.run
        Chapter13.SingletonBasics.run
        Chapter13.TypePatternMatching.run
    )
    "Chapter 13"
