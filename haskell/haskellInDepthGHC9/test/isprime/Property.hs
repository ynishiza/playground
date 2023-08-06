{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Property (properties, generateInteger) where

import Data.Foldable
import GHC.Exts (fromString)
import Hedgehog
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as H
import Prime

numTests :: Integer
numTests = 500

maxNumber :: Integral i => i
maxNumber = 10 ^ 5

toLabelName :: Show a => a -> LabelName
toLabelName = fromString . show

generateInteger :: Gen Integer
generateInteger = toInteger <$> H.int (H.linear 2 maxNumber)

property_ :: PropertyT IO () -> Property
property_ = withTests 100 . property

properties :: Group
properties = $$discover

prop_EachPrimalityTestProducesTheSameValue :: Property
prop_EachPrimalityTestProducesTheSameValue =
  property_ $ do
    i <- forAll generateInteger
    coversNumbers i maxNumber
    isPrime1 i === isPrime i
    isPrime1 i === isPrimeV2 i

prop_EachPrimalityListProducesTheSameValue :: Property
prop_EachPrimalityListProducesTheSameValue =
  property_ $ do
    l <- forAll $ H.int (H.linear 10 (10 ^ 4))
    coversNumbers l (10 ^ 4)
    take 10 (drop l primeList) === take 10 (drop l primeListV2)

coversNumbers :: (Show i, Integral i, Monad m) => i -> i -> PropertyT m ()
coversNumbers x upperBound = traverse_ checkCov [0, step .. upperBound - step]
  where
    checkCov lower =
      let upper = lower + step
       in cover 1 (toLabelName lower <> "," <> toLabelName upper) $ lower < x && x < upper
    step = upperBound `div` 4
