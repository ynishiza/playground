module Template.Parse (trivial) where

import Control.Arrow ((>>>))
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote

trivial :: QuasiQuoter
trivial =
  QuasiQuoter
    { quoteExp = parseWith parseExp,
      quoteDec = parseWith parseDecs,
      quoteType = parseWith parseType,
      quotePat = parseWith parsePat
    }

parseWith :: (String -> Either String a) -> String -> Q a
parseWith parser = parser
    >>> either
      (fail . ("Failed to parse:" <>))
      pure
