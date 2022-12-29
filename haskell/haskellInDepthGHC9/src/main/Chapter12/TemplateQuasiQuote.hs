module Chapter12.TemplateQuasiQuote (str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str =
  QuasiQuoter
    { quoteExp = litE . StringL
    }
