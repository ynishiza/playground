{-# LANGUAGE QuasiQuotes #-}

module Chapter12.TemplateQuasiQuoteUse
  ( x,
    myDynamicOne,
  )
where

import Chapter12.TemplateQuasiQuote

x :: String
x =
  [str| abc
def|]

[str|myDynamicOne|]
