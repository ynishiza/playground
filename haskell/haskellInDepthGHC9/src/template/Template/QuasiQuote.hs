{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Template.QuasiQuote (str, simple) where

import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str =
  QuasiQuoter
    { quoteExp = litE . StringL,
      quoteDec = undefined,
      quotePat = undefined,
      quoteType = undefined
    }

simple :: QuasiQuoter
simple =
  QuasiQuoter
    { quoteExp = litE . StringL,
      quoteDec = \s ->
        ( do
            sig <- sigD (mkName s) [t|Int|]
            value <- funD (mkName s) [clause [] (normalB [|1|]) []]
            return [sig, value]
        ),
      quotePat = varP . mkName,
      quoteType = conT . mkName
    }
