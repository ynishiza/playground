{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec (spec) where

import Data.Attoparsec.ByteString qualified as A
import Data.ByteString.Char8 qualified as B
import GHC.Base
import GHC.Generics
import Template.Parse
import Template.Printf
import Template.PrintfParser qualified as P
import Template.QuasiQuote
import Template.Reify
import Template.Tuple
import Template.Zip
import Test.Hspec

data Alpha = A | B | C | D | E | F | G
  deriving (Show, Eq, Bounded, Enum)

$[d|
   myValue :: Int
   myValue = 1
   |]

myId :: $[t|Int -> Int|]
myId $[p|x|] = $[|x|]

[simple|someValue|]

myId2 :: [simple|Int|] -> Int
myId2 [simple|x|] = x * 100

[trivial|
  someTrivialValue :: Int
  someTrivialValue = 12
 |]

someTrivialX :: [trivial|Int -> Int|]
someTrivialX [trivial|x|] =
  [trivial| 2 * x |]

spec :: Spec
spec = describe "Template" $ do
  it "basic" $ do
    myValue `shouldBe` 1
    myId 1 `shouldBe` 1

  describe "Reify" $ do
    it "Gets name info" $ do
      let infos :: [(String, String)]
          infos =
            [ $(getNameInfo ''Show),
              $(getNameInfo 'show),
              $(getNameInfo ''Bool),
              $(getNameInfo ''GHC.Generics.Rep),
              $(getNameInfo ''GHC.Base.Int#),
              $(getNameInfo 'True),
              $(getNameInfo 'id)
            ]
      infos
        `shouldBe` [ ("class", "Show"),
                     ("class method", "show"),
                     ("type", "Bool"),
                     ("type family", "Rep"),
                     ("primitive type constructor", "Int#"),
                     ("data", "True"),
                     ("variable", "id")
                   ]

    describe "ithOfTuple" $ do
      it "Gets ith of tuple" $ do
        $(ithOfTuple 2 0) (2 :: Int, True) `shouldBe` 2
        $(ithOfTuple 2 1) (2 :: Int, True) `shouldBe` True
        $(ithOfTuple 3 0) ('a', "Hello" :: String, True) `shouldBe` 'a'
        $(ithOfTuple 3 1) ('a', "Hello" :: String, True) `shouldBe` "Hello"
        $(ithOfTuple 3 2) ('a', "Hello" :: String, True) `shouldBe` True

    describe "zipN" $ do
      it "zips" $ do
        $(zipN 2)
          [A, B]
          [1 :: Int ..]
          `shouldBe` [(A, 1), (B, 2)]
        $(zipN 2)
          [A .. E]
          [1 :: Int ..]
          `shouldBe` [(A, 1), (B, 2), (C, 3), (D, 4), (E, 5)]
        $(zipN 3)
          [A, B]
          [1 :: Int ..]
          ['a' ..]
          `shouldBe` [(A, 1, 'a'), (B, 2, 'b')]
        $(zipN 10)
          [A, B]
          [1 :: Int ..]
          [2 :: Int ..]
          [3 :: Int ..]
          [4 :: Int ..]
          [5 :: Int ..]
          [6 :: Int ..]
          [7 :: Int ..]
          [8 :: Int ..]
          [9 :: Int ..]
          `shouldBe` [(A, 1, 2, 3, 4, 5, 6, 7, 8, 9), (B, 2, 3, 4, 5, 6, 7, 8, 9, 10)]

        $get1 `shouldBe` (1 :: Int)

    describe "printf" $ do
      let parse parser s = A.parseOnly parser (B.pack s)

      it "parses %d" $ do
        parse P.parseD "%d" `shouldBe` Right P.D

        parse P.parseD "d" `shouldBe` Left "D: string"
        parse P.parseD "%" `shouldBe` Left "D: not enough input"

      it "parses %s" $ do
        parse P.parseS "%s" `shouldBe` Right P.S

        parse P.parseS "s" `shouldBe` Left "S: string"
        parse P.parseS "%" `shouldBe` Left "S: not enough input"

      it "parses literal" $ do
        parse P.parseL "hello" `shouldBe` Right (P.Literal "hello")
        parse P.parseL " hello " `shouldBe` Right (P.Literal " hello ")
        parse P.parseL "hello%d" `shouldBe` Right (P.Literal "hello")

        parse P.parseL "%shello" `shouldBe` Left "Literal > literal char: Failed reading: Encountered format S"
        parse P.parseL "" `shouldBe` Left "Literal > literal char: not enough input"

      it "printf" $ do
        $(printf "hello") `shouldBe` ("hello" :: String)

        $(printf "hello %s") "DEF" `shouldBe` ("hello DEF" :: String)
        $(printf "%s hello") "ABC" `shouldBe` ("ABC hello" :: String)
        $(printf "%s hello %s") "ABC" "DEF" `shouldBe` ("ABC hello DEF" :: String)
        $(printf "%d hello %d") 1 2 `shouldBe` ("1 hello 2" :: String)

    describe "QuasiQuoter" $ do
      it "str" $ do
        [str|hello world|] `shouldBe` ("hello world" :: String)

      it "simple" $ do
        someValue `shouldBe` 1
        [simple|hello world|] `shouldBe` "hello world"
        myId2 10 `shouldBe` 1000

      it "with parser" $ do
        someTrivialValue `shouldBe` 12
        someTrivialX 10 `shouldBe` 20
