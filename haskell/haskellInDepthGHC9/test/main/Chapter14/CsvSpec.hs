module Chapter14.CsvSpec
  ( spec,
  )
where

import Chapter14.Chapter14_2_2
import Control.Monad.Trans.Resource
import Data.Function
import Streaming.Prelude qualified as S
import Test.Hspec

sampleCsv :: FilePath
sampleCsv = "./src/data/quotes.csv"

spec :: Spec
spec = describe "Chapter 14.2.2" $ do
  it "parses a csv file" $ do
    parsed <-
      runResourceT $
        readCsv sampleCsv
          & S.toList_
          & (head <$>)

    length parsed `shouldBe` 107
