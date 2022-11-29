module Modules.TestHspec
  ( main,
    runTests,
    specs1,
  )
where

import Control.Exception
import System.Random
import Test.Hspec
import TestUtils

runTests :: TestState
runTests =
  createTest
    ( do
        hspec mainSpecsAround
    )
    "TestHspec"

main :: IO ()
main = hspec mainSpecs

mainSpecsAround :: SpecWith ()
mainSpecsAround = f `aroundAll_` specs1
  where
    onBefore = putStrLn "Before"
    onAfter = putStrLn "After"
    f = bracket_ onBefore onAfter

mainSpecs :: SpecWith ()
mainSpecs =
  beforeAll_ onBefore $
    afterAll_ onAfter $
      before_ onBeforeEach $
        after_ onAfterEach specs1
  where
    onBefore = putStrLn "Before"
    onAfter = putStrLn "After"
    onBeforeEach = putStrLn "BeforeEach"
    onAfterEach = putStrLn "AfterEach"

randomInt :: IO Int
randomInt = getStdRandom uniform

specs1 :: Spec
specs1 = describe "My tests" $ do
  describe "Basic" $ do
    it "1 == 1" $
      1 `shouldBe` 1

    it "1 /= 2" $
      1 `shouldNotBe` 2

  describe "IO" $ do
    it "use IO" $ do
      putStrLn "Enter 1"
      z <- getLine
      z `shouldBe` "1"

    it "Random" $ do
      x <- randomInt
      y <- randomInt
      x `shouldNotBe` y

  describe "Pending" $ do
    it "TODO 1" $ do
      pending

    it "TODO 2" $ do
      pendingWith "TODO"

  before testSetup $
    after testCleanup $
      describe "Tests with test args" $ do
        it "is not zero" $ \v -> do
          v `shouldNotBe` 0
  where
    testSetup :: IO Int
    testSetup = getStdRandom uniform
    testCleanup v = putStrLn $ "tested with=" ++ show v
