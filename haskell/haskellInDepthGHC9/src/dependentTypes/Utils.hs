{-# LANGUAGE TemplateHaskell #-}
module Utils where
import Language.Haskell.TH
import Language.Haskell.TH.Quote

evanShow = QuasiQuoter {
  quoteExp = undefined
                       }



