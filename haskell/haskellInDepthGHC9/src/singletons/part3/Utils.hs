{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Utils
  ( printHeader,
    printWithLabel,
    showType,
    showProof,
  )
where

import Data.Singletons.Decide
import Data.Typeable
import Fmt

printHeader :: String -> IO ()
printHeader v = fmtLn $ "\n----" +| v |+ "----"

printWithLabel :: Show a => String -> a -> IO ()
printWithLabel n v = fmt $ nameF (build n) (build $ show v)

showType :: forall p s. (Typeable s, Typeable p) => p s -> String
showType x = show $ typeOf x

showProof :: forall a. Typeable a => Decision a -> String
showProof d = fmt $ p |+ "" +| showType d |+ ""
  where
    p :: Builder
    p = case d of Proved _ -> "PROVED\t\t"; _ -> "DISPROVED\t"

-- ==================== Q1 ====================
