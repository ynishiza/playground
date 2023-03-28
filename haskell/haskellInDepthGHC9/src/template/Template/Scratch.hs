{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Template.Scratch
  ( mk,
    mkFst',
    y,
  )
where

import Control.Monad
import Language.Haskell.Meta.Parse
import Language.Haskell.TH

mk :: Quote m => String -> m Dec
mk n = funD (mkName n) [clause [] (normalB [|1|]) []]

mkFst' :: Quote m => m Dec
mkFst' =
  funD
    (mkName "fst'")
    [ clause
        [[p|(a, b)|]]
        (normalB [|a|])
        []
    ]

y :: Quote m => m [Dec]
y = traverse mk ["abc", "def"]
