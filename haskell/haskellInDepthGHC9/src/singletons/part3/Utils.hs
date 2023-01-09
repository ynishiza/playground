{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( printHeader,
    printWithLabel,
  )
where

import Fmt

printHeader :: String -> IO ()
printHeader v = fmtLn $ "\n----" +| v |+ "----"

printWithLabel :: Show a => String -> a -> IO ()
printWithLabel n v = fmt $ nameF (build n) (build $ show v)
