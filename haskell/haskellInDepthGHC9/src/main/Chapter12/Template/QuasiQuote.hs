{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter12.Template.QuasiQuote (str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str =
  QuasiQuoter
    { quoteExp = litE . StringL,
      quoteDec = \s -> do
        let name = mkName s
        signature <- sigD name [t|Int|]
        definition <- funD name [clause [] (normalB [|1 :: Int|]) []]
        return [signature, definition],
      quoteType = undefined,
      quotePat = undefined
    }
