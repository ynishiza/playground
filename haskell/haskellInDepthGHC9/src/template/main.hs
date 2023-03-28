{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import GHC.Base
import GHC.Generics
import Template.QuasiQuote
import Template.Scratch
import Template.Reify
import Template.Tuple

main = undefined

$y
$(pure (: []) <*> mkFst')

v1 :: Int
v1 = $(ithOfTuple 2 0) (2 :: Int, True)

-- Error: 2 <= 2
-- z = $(ithOfTuple 2 2) (2, True)

v2 :: Bool
v2 = $(ithOfTuple 2 1) (2 :: Int, True)

v3 :: Char
v3 = $(ithOfTuple 3 0) ('a', "Hello", True)
