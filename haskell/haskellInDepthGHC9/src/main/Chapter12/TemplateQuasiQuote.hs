{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Chapter12.TemplateQuasiQuote (str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str =
  QuasiQuoter
    { quoteExp = litE . StringL,
      quoteDec = \s -> do 
        d <- funD (mkName s) [clause [] (normalB [|1 :: Int|]) []]
        return [d]
    }
