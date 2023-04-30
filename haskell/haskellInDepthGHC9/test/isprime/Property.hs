{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Property (properties, generateInteger) where

import Hedgehog
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as H
import Prime

generateInteger :: Gen Integer
generateInteger = toInteger <$> H.int (H.linear 2 (10 ^ (5 :: Int)))

properties :: Group
properties =
  Group
    { groupName = "prime properties",
      groupProperties =
        [ ( "Each primality test produces the same value",
            property $ do
              i <- forAll generateInteger
              isPrime1 i === isPrime i
              isPrime1 i === isPrimeV2 i
          ),
          ( "Each prime list produeces the same value",
            property $ do
              l <- forAll $ H.int (H.linear 10 (10^4))
              take 10 (drop l primeList) === take 10 (drop l primeListV2)
          )
        ]
    }
