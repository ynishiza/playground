{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chapter2_1
  ( test,
  )
where

import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Fmt
import GHC.Exts
import GHC.Generics
import qualified Hedgehog as G
import Hedgehog.Gen
import Hedgehog.Range
import Radar
import System.Random
import System.Random.Stateful
import Utils
test :: TestState
test =
  createChapterTest
    "2"
    "Radar"
    ( do
        G.checkSequential radarProperties >>= assertIsEqual True
        oldTests
    )

oldTests :: IO ()
oldTests = do
  dirPairs <- getRandomDirectionsSequence 1000 2
  let toPair [d1, d2] = (d1, d2)
      toPair _ = error "undefined"
   in testRotateOrientDuality $ toPair <$> dirPairs

  dirSequences <- getRandomDirectionsSequence 100 10
  testRotateOrientDualityMany dirSequences

deriving instance Generic Direction

instance UniformRange Direction where
  uniformRM (low, high) g = do
    v <- uniformRM (fromEnum low :: Int, fromEnum high) g
    let y = mod v (getEnumLength (Proxy @Direction))
    pure $ toEnum y

instance Uniform Direction where
  uniformM = uniformRM (minBound, maxBound)

randomDirection :: IO Direction
randomDirection = getStdRandom uniform

getRandomDirections :: Int -> IO [Direction]
getRandomDirections n = replicateM n randomDirection

getRandomDirectionsSequence :: Int -> Int -> IO (NonEmpty [Direction])
getRandomDirectionsSequence n k = fromList <$> replicateM n (getRandomDirections k)

testRotateOrientDualityMany :: NonEmpty [Direction] -> IO ()
testRotateOrientDualityMany dirSeqs = do
  printBanner "testRotateOrientDualityMany start"
  traverse_ testOne dirSeqs
  fmtLn $
    "Number of pairs=" +| L.length dirSeqs |+ "\n"
      +| "sample:" +|| L.take 10 dirSeqs ||+ "..."
  printBanner "testRotateOrientDualityMany done"
  where
    testOne :: [Direction] -> IO ()
    testOne dirSeq@(h : _) = do
      let rots = orientMany dirSeq
          dirSeq2 = rotateManyInSteps h rots
      assertIsEqualSilent dirSeq dirSeq2
      return ()
    testOne _ = error "empty"

testRotateOrientDuality :: NonEmpty (Direction, Direction) -> IO ()
testRotateOrientDuality dirPairs = do
  printBanner "testRotateOrientDuality start"
  traverse_ testOne dirPairs
  fmtLn $
    "Number of pairs=" +| L.length dirPairs |+ "\n"
      +| "sample:" +|| L.take 10 dirPairs ||+ "..."
  printBanner "testRotateOrientDuality done"
  where
    testOne :: (Direction, Direction) -> IO ()
    testOne (d1, d2) = do
      let rot = orient d1 d2
          dirRes = rotate rot d1
      assertIsEqualSilent d2 dirRes

genDirection :: G.Gen Direction
genDirection = toEnum . fromIntegral <$> int8 (linear 0 3)

genManyDirections :: G.Gen [Direction]
genManyDirections = list (linear 10 100) genDirection

radarProperties :: G.Group
radarProperties =
  G.Group
    { groupName = "Radar",
      groupProperties =
        [ ( "duality: rotate (orient d1 d2) d1 === d2",
            G.property $ do
              d1 <- G.forAll genDirection
              d2 <- G.forAll genDirection
              let rot = orient d1 d2
              d2 G.=== rotate rot d1
          ),
          ( "duality: (d0:ds) === rotateManyInSteps d0 (orientMany (d0:ds))",
            G.property $ do
              d0 <- G.forAll genDirection
              ds <- G.forAll genManyDirections
              let rots = orientMany (d0 : ds)
              (d0 : ds) G.=== rotateManyInSteps d0 rots
              pure ()
          )
        ]
    }

