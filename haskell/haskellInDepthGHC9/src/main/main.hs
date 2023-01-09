{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Chapter11.Base qualified
import Chapter12.Base qualified
import Chapter13.Base qualified
import Chapter1_4 qualified
import Chapter2_1 qualified
import Chapter5_1_3 qualified
import Chapter5_2_1 qualified
import Chapter5_3_1 qualified
import Chapter6_2 qualified
import Chapter7_2 qualified
import Chapter7_3 qualified
import Chapter9_2 qualified
import Control.Exception
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fmt
import Text.Read
import Utils

main :: IO ()
main =
  do
    let chapterList :: [(T.Text, Int)]
        chapterList =
          zip
            [ 
              "Chapter1",
              "Chapter2",
              "Chapter5",
              "Chapter6",
              "Chapter7.2",
              "Chapter7.3",
              "Chapter9.2",
              "Chapter11.1",
              "Chapter12",
              "Chapter13"
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
      Just 10 -> runTest Chapter13.Base.run
      Just _ -> error $ "Unknown chapter" +| res |+ ""
      Nothing -> error $ "Unknown chapter" +| res |+ ""
    `catch` handler
  where
    handler :: SomeException -> IO ()
    handler e = fmtLn ("error:" +|| e ||+ "") >> throw e
