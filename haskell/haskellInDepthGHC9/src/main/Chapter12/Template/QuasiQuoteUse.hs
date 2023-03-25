{-# LANGUAGE QuasiQuotes #-}

module Chapter12.Template.QuasiQuoteUse
  ( x,
    myDynamicOne,
  )
where

import Chapter12.Template.QuasiQuote

x :: String
x =
  [str| abc
def|]

[str|myDynamicOne|]
