{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import qualified Chapter1_4
import qualified Chapter2_1
import qualified Chapter5_1_3
import qualified Chapter5_2_1
import qualified Chapter5_3_1
import qualified Chapter6_2
import qualified Chapter7_2
import qualified Chapter7_3
import qualified Chapter9_2
import qualified Chapter12.Base
import qualified Chapter11.Base
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception
import Fmt
import Text.Read
import Utils

main :: IO ()
main = do
  let chapterList :: [(T.Text, Int)]
      chapterList =
        zip
          [ "Chapter1",
            "Chapter2",
            "Chapter5",
            "Chapter6",
            "Chapter7.2",
            "Chapter7.3",
            "Chapter9.2",
            "Chapter11.1",
            "Chapter12"
          ]
          [1 ..]

  putStrLn $
    fmt $
      "Choose chapter to run\n"
        +| blockListF' "" (\(name, i) -> i |+ ":" +| name |+ "") chapterList

  res <- TIO.getLine
  case readMaybe (T.unpack res) of
    Just 1 -> runTest $ do
      Chapter1_4.run
    Just 2 -> runTest $ do
      Chapter2_1.run
    Just 3 -> runTest $ do
      Chapter5_1_3.run
      Chapter5_2_1.run
      Chapter5_3_1.run
    Just 4 -> do
      Chapter6_2.run
    Just 5 -> runTest Chapter7_2.run
    Just 6 -> runTest Chapter7_3.run
    Just 7 -> runTest Chapter9_2.run
    Just 8 -> runTest Chapter11.Base.run
    Just 9 -> runTest Chapter12.Base.run
    Just _ -> error $ "Unknown chapter" +| res |+ ""
    Nothing -> error $ "Unknown chapter" +| res |+ ""

  `catch` handler
    where handler :: SomeException -> IO ()
          handler e = fmtLn ("error:" +||e||+"") >> throw e
