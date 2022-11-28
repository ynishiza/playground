{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec (main, mainTasty, mainHspec) where

import Data.Bits
import qualified Data.Text as T
import Data.Typeable
import Data.Word
import Fmt
import IPLookup
import IPParse
import System.Random
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.Hspec
import Utils

-- 192.168.3.15
testIPWord32 :: Word32
testIPWord32 = 15 + shift 3 8 + shift 168 16 + shift 192 24

randomTestSize :: Int
randomTestSize = 100

randomFile :: FilePath
randomFile = "./random.iplookup-test.txt"

main :: IO ()
main = do
  writeFile randomFile "# Random values used by ./test/iplookup/Spec.hs\n"
  mainHspec
  "Random values logged in:" +| randomFile |+ ""

mainTasty :: IO ()
mainTasty = 
  testSpec "IP" specs >>= defaultMain

mainHspec :: IO ()
mainHspec = hspec specs

logRandom :: (Typeable a, Show a) => IO a -> IO a
logRandom io = do
  v <- io
  appendFile randomFile $ (typeOf v ||+ "\t") <> (v ||+ "\n")
  return v

randomByte :: IO Byte
randomByte = logRandom $ getStdRandom genWord8

randomByteSeq :: IO ByteSeq
randomByteSeq = logRandom $ replicateM 4 randomByte

randomIP :: IO IP
randomIP = logRandom $ getStdRandom (uniform @StdGen @IP)

randomIPRange :: IO IPRange
randomIPRange = logRandom $ getStdRandom (uniform @StdGen @IPRange)

manyRandom :: Monad m => m a -> m [a]
manyRandom = replicateM randomTestSize

assertManyWith :: (a -> Expectation) -> IO [a] -> Expectation
assertManyWith t x = do
  vs <- x
  mconcat $ t <$> vs

instance Uniform IP where
  uniformM = (IP <$>) . uniformM

instance UniformRange IP where
  uniformRM (IP l, IP h) = (IP <$>) . uniformRM @Word32 (l, h)

instance Uniform IPRange where
  uniformM g = do
    ip1 <- uniformM g
    ip2 <- uniformM g
    when (ip1 == ip2) shouldNeverHappen
    return $ if ip1 < ip2 then IPRange ip1 ip2 else IPRange ip2 ip1

specs :: Spec
specs = describe "iplookup" $ do
  describe "serialization" $ do
    it "is invertible" $ assertManyWith (\x -> unserializeIP (serializeIP x) `shouldBe` x) (manyRandom randomByteSeq)

  describe "buildIP" $ do
    it "builds from zero" $
      buildIP [0, 0, 0, 0] `shouldBe` IP 0

    it "builds from one" $
      buildIP [0, 0, 0, 1] `shouldBe` IP 1

    it "builds from localhost" $
      buildIP [127, 0, 0, 1] `shouldBe` IP (1 + shift 127 24)

    it "builds from arbitrary address" $
      buildIP [192, 168, 3, 15] `shouldBe` IP testIPWord32

  let test :: T.Text -> [Maybe IP] -> Expectation
      test x [v1, v2, v3, v4] = do
        parseIP (x |+ ".0.0.0") `shouldBe` v1
        parseIP ("0." +| x |+ ".0.0") `shouldBe` v2
        parseIP ("0.0." +| x |+ ".0") `shouldBe` v3
        parseIP ("0.0.0." +| x |+ "") `shouldBe` v4
      test _ _ = shouldNeverHappen
      testFail = flip test (replicate 4 Nothing)

  describe "parseIP" $ do
    it "is invertible" $ assertManyWith (\ip -> parseIP (unparseIP ip) `shouldBe` Just ip) (manyRandom randomIP)

    it "parses zero" $
      parseIP "0.0.0.0" `shouldBe` Just (IP 0)

    it "parses one" $
      parseIP "0.0.0.1" `shouldBe` Just (IP 1)

    it "parses the max IP address" $
      parseIP "255.255.255.255" `shouldBe` Just (IP maxBound)
    it "parses an arbitrary IP address" $
      parseIP "192.168.3.15" `shouldBe` Just (IP testIPWord32)

    it "fails to parse non-matching size" $ do
      parseIP "0" `shouldBe` Nothing
      parseIP "0.0" `shouldBe` Nothing
      parseIP "0.0.0" `shouldBe` Nothing
      parseIP "0.0.0.0.0" `shouldBe` Nothing
      parseIP "0.0.0.0.0.0" `shouldBe` Nothing

    it "fails to parse a large value" $
      testFail "1000"

    it "fails to parse non-numeric" $ do
      testFail "x"
      testFail "@"
      testFail "-"

    it "fails to parse negative" $
      testFail "-1"

  describe "parseIPRange" $ do
    it "invertible" $ assertManyWith (\x -> parseIPRange (unparseIPRange x) `shouldBe` Just x) (manyRandom randomIPRange)
    it "parses a range" $
      parseIPRange "0.0.0.0,0.0.0.1" `shouldBe` Just (IPRange (IP 0) (IP 1))

    it "parses with whitespaces" $ do
      parseIPRange "0.0.0.0,  0.0.0.1" `shouldBe` Just (IPRange (IP 0) (IP 1))
      parseIPRange "0.0.0.0,  0.0.0.1" `shouldBe` Just (IPRange (IP 0) (IP 1))

    it "fails to parse if one of the IPs is invalid" $ do
      parseIPRange "0.0.0.0,0.0.0.x" `shouldBe` Nothing
      parseIPRange "x.0.0.0,0.0.0.0" `shouldBe` Nothing

    it "fails to parse if the range is invalid" $ do
      parseIPRange "0.0.0.0,0.0.0.1" `shouldBe` Just (IPRange (IP 0) (IP 1))
      parseIPRange "0.0.0.1,0.0.0.0" `shouldBe` Nothing

  describe "parseIPRangeDB" $ do
    it "parses" $ do
      let txt =
            T.unlines
              [ "0.0.0.0,0.0.1.1",
                "127.0.0.1,128.0.0.1",
                "10.0.3.1,11.2.0.1"
              ]
      parseIPRangeDB txt
        `shouldBe` Right
          ( IPRangeDB
              [ IPRange (buildIP [0, 0, 0, 0]) (buildIP [0, 0, 1, 1]),
                IPRange (buildIP [127, 0, 0, 1]) (buildIP [128, 0, 0, 1]),
                IPRange (buildIP [10, 0, 3, 1]) (buildIP [11, 2, 0, 1])
              ]
          )

    it "provides the line number if it fails to parse" $ do
      let txt =
            [ "0.0.0.0,1.1.1.1",
              "0.0.0.0,1.1.1.1",
              "0.0.0.0,1.1.1.1",
              "0.0.0.0,1.1.1.1",
              "0.0.0.0,x.1.1.1",
              "0.0.0.0,1.1.1.1",
              "0.0.0.0,1.1.1.1"
            ]
      parseIPRangeDB (T.unlines txt)
        `shouldBe` Left (ParserError 4)
      parseIPRangeDB (T.unlines $ drop 1 txt)
        `shouldBe` Left (ParserError 3)
      parseIPRangeDB (T.unlines $ drop 2 txt)
        `shouldBe` Left (ParserError 2)
      parseIPRangeDB (T.unlines $ drop 4 txt)
        `shouldBe` Left (ParserError 0)
      parseIPRangeDB (T.unlines $ drop 5 txt)
        `shouldBe` Right
          ( IPRangeDB
              [ IPRange (IP 0) (buildIP [1, 1, 1, 1]),
                IPRange (IP 0) (buildIP [1, 1, 1, 1])
              ]
          )
    describe "lookupIP" $ do
      let db =
            IPRangeDB
              [ IPRange (IP 0) (buildIP [1, 1, 1, 1]),
                IPRange (buildIP [10, 8, 1, 3]) (buildIP [10, 8, 1, 255]),
                IPRange (buildIP [10, 9, 1, 3]) (buildIP [10, 9, 1, 255]),
                IPRange (buildIP [25, 8, 1, 3]) (buildIP [30, 8, 1, 255])
              ]
      it "looksup IP" $ do
        minIP `shouldSatisfy` lookupIP db
        buildIP [10, 8, 1, 10] `shouldSatisfy` lookupIP db
        buildIP [25, 8, 4, 10] `shouldSatisfy` lookupIP db

      it "reports an error if it fails on lookup" $ do
        maxIP `shouldNotSatisfy` lookupIP db
        buildIP [10, 8, 1, 1] `shouldNotSatisfy` lookupIP db
        buildIP [30, 8, 2, 1] `shouldNotSatisfy` lookupIP db
