{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter11.TypeFamilySynonyms
  ( run,
    CanSimplify (..),
    CanWiden (..),
  )
where

import Chapter11.TypeFamilySynonymsEg1
import Chapter11.TypeFamilySynonymsEg2
import Fmt
import Utils

data TestValue
  = forall a.
    ( Show a,
      CanSimplify a,
      Show (Simplify a),
      CanWiden a,
      Show (Widen a)
    ) =>
    MkTestValue a

run :: TestState
run =
  createChapterTest
    "11.3"
    "TypeFamilySynonyms"
    ( do
        printBanner "Simply"
        let testValues =
              [ ("Int", MkTestValue (1 :: Int)),
                ("Double", MkTestValue (1.1 :: Double)),
                ("Bool", MkTestValue True),
                ("Char", MkTestValue 'a'),
                ("String", MkTestValue ("Hello" :: String))
              ]
        _ <-
          let f :: (Show a, Show (Simplify a), CanSimplify a) => String -> a -> Builder
              f t s = "[" +| t |+ "] " +|| s ||+ "\t-simplify->\t" +|| simplify s ||+ ""
           in traverse (\(x, MkTestValue v) -> fmtLn $ f x v) testValues

        _ <-
          let f :: (Show a, Show (Widen a), CanWiden a) => String -> a -> Builder
              f t s = "[" +| t |+ "] " +|| s ||+ "\t-widen->\t" +|| widen s ||+ ""
           in traverse (\(x, MkTestValue v) -> fmtLn $ f x v) testValues
        pure ()
    )
