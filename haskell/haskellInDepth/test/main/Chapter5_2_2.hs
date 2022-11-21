{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Chapter5_2_2 (testShuntingYard) where

import Chapter5_2_2_Parser
import Control.Monad.Except
import Data.Foldable
import Fmt
import Utils

testShuntingYard :: TestState
testShuntingYard =
  createChapterTest
    "5.2.2"
    "Shunting yard"
    ( do
        printBanner "Shunting yard algorithm"
        let printResult r = case r of
              Left (SYError s msg) ->
                "ERROR\nmessage:" +| msg |+ "\n"
                  +| logState s |+ "\n"
              Right (e, s) ->
                "SUCCESS\nexpression:" +|| e ||+ "\n"
                  +| logState s |+ "\n"

        -- Success tests
        let testOne :: Expr Int -> String -> IO ()
            testOne expected expr = do
              "expression=" +| expr |+ "\n"
                +| indentF 2 (printResult res) |+ "\n"
              case res of
                Right (e, (stack, _)) -> do
                  assertIsEqual e expected
                  assertIsEqual stack []
                Left _ -> error "Fail"
              where
                res = parse expr
            testOneCase (e, expr, alts) = traverse_ (testOne e) (expr : alts)
         in traverse_ testOneCase parseSuccessCases

        -- Fail tests
        let testOne (expectedMsg, expr) = do
              "expression=" +| expr |+ "\n"
                +| indentF 2 (printResult res) |+ "\n"
              case res of
                Right _ -> error "Fail"
                Left (SYError _ msg) -> do
                  assertIsEqual msg expectedMsg
              where
                res = parse expr
         in traverse_ testOne parseFailCases

        printBanner "catchError test"
        let 
          p = catchError (createParser "2a") (\e -> trace (show e) $ createParser "2") 
          in printResult $ processParser p
        pure ()
    )

parseSuccessCases :: [(Expr Int, String, [String])]
parseSuccessCases =
  [ (one, "1", ["(1)", "   1", "(   1   )"]),
    (two, "2", ["2"]),
    (Lit 10000, "10000", []),
    (Lit (-1), "-1", []),
    (Add one two, "1+2", ["(1)+2", "1+(2)", "(1+2)", "1 + 2"]),
    (Mult one two, "1*2", ["(1)*2", "1*(2)", "(1*2)", "1 * 2"]),
    -- Chain
    (Add one (Add two (Add three four)), "1+2+3+4", ["(1+2+3+4)", "1+(2+3+4)", "1+(2+(3+4))", "1+2+(3+4)"]),
    (Add (Add (Add one two) three) four, "((1+2)+3)+4", []),
    (Add (Add one two) (Add three four), "(1+2)+(3+4)", []),
    (Mult one (Mult two (Mult three four)), "1*2*3*4", []),
    -- Mix
    (Mult (Add one two) three, "(1+2)*3", ["(1+2)*(3)"]),
    (Mult one (Add two three), "1*(2+3)", ["(1)*(2+3)"]),
    (Mult (Add one two) (Add three four), "(1+2)*(3+4)", []),
    -- associativity
    (Add (Mult two (Add one two)) three, "2*(1+2)+3", []),
    (Add three (Mult two (Add one two)), "3+2*(1+2)", []),
    (Add (Mult one two) (Add (Mult two three) (Mult three four)), "1*2+2*3+3*4", ["(1*2)+(2*3)+(3*4)"]),
    (Mult one (Mult (Add two two) (Mult (Add three three) four)), "1*(2+2)*(3+3)*4", []),
    -- Deep nested
    (Mult two (Add (Mult two (Add (Mult two (Add (Mult two (Add one one)) one)) one)) one), "2*(2*(2*(2*(1+1)+1)+1)+1)", [])
  ]
  where
    one = Lit 1
    two = Lit 2
    three = Lit 3
    four = Lit 4

parseFailCases :: [(String, String)]
parseFailCases =
  [ ("Failed to read token. token=a", "a"),
    ("Not enough output. token=+", "1+"),
    ("Not enough output. token=+", "+1"),
    ("popStack: empty stack", "(1+1"),
    ("Failed to read token. token=)", "1+1)")
  ]
