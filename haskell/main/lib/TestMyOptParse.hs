{-# LANGUAGE OverloadedStrings #-}

module TestMyOptParse (runAll) where

import qualified MyOptParseWithExcept as PE
import qualified MyOptParse as P
import TestUtils

runAll :: TestState
runAll = do
  testMyOptParseWithExcept
  testMyOptParseSimple

testMyOptParseSimple :: TestState
testMyOptParseSimple = createTest (do
    let parser =
          (,,)
            <$> P.parseOption (P.createStringOption 'n' "name")
            <*> P.parseOption (P.Opt 'v' "value" Nothing P.readMaybe)
            <*> P.parseOption (P.createSimpleOption 'i' "int") ::
            P.ParserBase (String, Int, Int)
        required = ["-v", "10"]

    assertSuccessSimple parser (required ++ ["-n", "Yui", "-i", "1"]) ("Yui", 10, 1)
    assertSuccessSimple parser (required ++ ["--name", "Yui", "--int", "1"]) ("Yui", 10, 1)
    assertSuccessSimple parser (required ++ ["--int", "10"]) ("", 10, 10)
    assertSuccessSimple parser (required ++ ["--name", "Yui"]) ("Yui", 10, 0)
    assertSuccessSimple parser required ("", 10, 0)

    assertErrorSimple parser [] [] $ P.PRequierdOptionError  "Error in -v|--value:Missing required option"
    assertErrorSimple parser (required ++ ["-i", "a"]) ["-i", "a"] $ P.POptionReadError  1 "Error in -i|--int:Failed to parse value:a"
    assertErrorSimple parser (required ++ ["-i"]) ["-i"] $ P.POptionReadError  0 "Error in -i|--int:Missing option value"
    assertErrorSimple parser (required ++ ["-n"]) ["-v", "10", "-n"] $ P.POptionReadError  2 "Error in -n|--name:Missing option value"
    assertErrorSimple parser (required++["-a"]) ["-a"] $ P.PError  "Unused args:[-a]\n"
    return ()) "testMyOptParseSimple"

assertErrorSimple :: (Eq a, Show a) => P.ParserBase a -> [String] -> [String] -> P.ParserError -> IO ()
assertErrorSimple parser args pstate e =
  assertIsEqual (P.runParser parser args) (Left e, pstate)

assertSuccessSimple :: (Eq a, Show a) => P.ParserBase a -> [String] -> a -> IO ()
assertSuccessSimple parser args expected =
  assertIsEqual (fst $ P.runParser parser args) (Right expected)


testMyOptParseWithExcept :: TestState
testMyOptParseWithExcept = createTest (do
        let parser =
              (,,)
                <$> PE.parseOption (PE.createStringOption 'n' "name")
                <*> PE.parseOption (PE.Opt 'v' "value" Nothing PE.readMaybe)
                <*> PE.parseOption (PE.createSimpleOption 'i' "int") ::
                PE.ParserBase (String, Int, Int)
            required = ["-v", "10"]

        assertSuccessPE parser (required ++ ["-n", "Yui", "-i", "1"]) ("Yui", 10, 1)
        assertSuccessPE parser (required ++ ["--name", "Yui", "--int", "1"]) ("Yui", 10, 1)
        assertSuccessPE parser (required ++ ["--int", "10"]) ("", 10, 10)
        assertSuccessPE parser (required ++ ["--name", "Yui"]) ("Yui", 10, 0)
        assertSuccessPE parser required ("", 10, 0)

        assertErrorPE parser [] $ PE.PRequierdOptionError [] "Error in -v|--value:Missing required option"
        assertErrorPE parser (required ++ ["-i", "a"]) $ PE.POptionReadError ["-i", "a"] 1 "Error in -i|--int:Failed to parse value:a"
        assertErrorPE parser (required ++ ["-i"]) $ PE.POptionReadError ["-i"] 0 "Error in -i|--int:Missing option value"
        assertErrorPE parser (required ++ ["-n"]) $ PE.POptionReadError ["-v", "10", "-n"] 2 "Error in -n|--name:Missing option value"
        assertErrorPE parser (required++["-a"]) $ PE.PError ["-a"] "Unused args:[-a]\n"
        return ()) "testMyOptParseWithExcept"

assertErrorPE :: (Eq a, Show a) => PE.ParserBase a -> [String] -> PE.ParserError -> IO ()
assertErrorPE parser args e =
  assertIsEqual (PE.runParser parser args) (Left e)

assertSuccessPE :: (Eq a, Show a) => PE.ParserBase a -> [String] -> a -> IO ()
assertSuccessPE parser args expected =
  assertIsEqual (PE.runParser parser args) (Right expected)
