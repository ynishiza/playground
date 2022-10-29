{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use shows" #-}
{-# HLINT ignore "Use show" #-}
module TestReadShow (
  testReadShow,
  unitTestEquation
) where

import Debug.Trace (trace)
import Control.Monad
import Control.Arrow (first)
import Data.Foldable
import TestUtils

data MyBTree a = MyNode (MyBTree a) (MyBTree a) | MyLeaf a

instance Show a => Show (MyBTree a) where
  showsPrec d tree
    | (MyLeaf x) <- tree = showParen (d > 0) $ leafLabel . showsPrec 11 x
    | (MyNode left right) <- tree =
        showParen True $ showsPrec (d + 1) left . delim . showsPrec (d + 1) right
    where
      leafLabel = showString "Leaf "
      delim = showString " "

data Equation a = Sum (Equation a) (Equation a) | Product (Equation a) (Equation a) | Term a deriving (Eq)

instance Show a => Show (Equation a) where
  showsPrec d (Term x) = showsPrec d x
  showsPrec d eq
    | (Sum eq1 eq2) <- eq = showOperator 0 "+" eq1 eq2 -- sum has lowest precedence
    | (Product eq1 eq2) <- eq = showOperator 1 "*" eq1 eq2 -- product has high precedence
    where
      showOperator p symbol eq1 eq2 =
        -- add parenthesis if operator has higher precedence
        showParen (d > p) $ showsPrec p eq1 . showString symbol . showsPrec p eq2


type ReadsPrec a = Int -> ReadS a

readParenAlways :: ReadS a -> ReadS a
readParenAlways = readParen True

instance Read a => Read (Equation a) where
  readsPrec d text = readWithoutBrackets d text
    where
      -- readsPrec d text = readLeftmostTerm d text

      readWithoutBrackets _d _text = readLeftmostTerm _d _text ++ readLeftmostEq _d _text
      readLeftmostTerm _d = readLeftmost (readOneTerm _d) _d
      readLeftmostEq _d = readLeftmost (readParenAlways (readsPrec _d)) _d

      readLeftmost :: ReadS (Equation a) -> ReadsPrec (Equation a)
      -- readLeftmost = undefined
      readLeftmost readLeft _d _text = do
        (leftEq, leftRest) <- readLeft _text
        if null leftRest
          then -- then trace ("no right" ++ leftRest) $ return (leftEq, leftRest)
          -- else trace ("right" ++ leftRest) $ readRight leftEq _d leftRest
            return (leftEq, leftRest)
          else readRight leftEq _d leftRest
      readRight :: Equation a -> ReadsPrec (Equation a)
      readRight left _d "" = [(left, "")]
      readRight left _d _text =
        -- case:
        do
          (op, oprest) <- trace _text $ lex _text
          guard $ isOp op
          let opPrec = getOpPrec op
          -- trace (op ++ oprest) $ guard $ isGood _d op
          guard $ isGood _d op
          (right, rightrest) <- readsPrec opPrec oprest
          return (applyOp op left right, rightrest)
          ++ do
            (op, _) <- lex _text
            guard $ not $ isGood _d op
            return (left, _text)

      isGood _d op = isOp op && getOpPrec op >= _d
      readOneTerm :: ReadsPrec (Equation a)
      readOneTerm _d _text = first Term <$> readValue _d _text
      readValue :: ReadsPrec a
      readValue = readsPrec
      isOp op = op == "+" || op == "*"
      applyOp op v1 v2 | op == "+" = Sum v1 v2 | otherwise = Product v1 v2
      getOpPrec op | op == "+" = 0 | otherwise = 1

-- readsPrec d text =
--   let result =
--         readParen False (readFirstTerm d) text
--           ++ readParen False (readFirstEquation d) text
--    in -- ++ (readFirstEquation d) text
--       -- ++ (readFirstTerm d) text
--       if null result then result else [minimumBy (\x y -> compare (length $ snd x) (length $ snd y)) result]
--   where

--     readFirstTerm d = readWithFirstBase (readTerm d) d
--     readFirstEquation d = readWithFirstBase (readParen True $ readsPrec d) d

--     readWithFirstBase :: ReadS (Equation a) -> Int -> ReadS (Equation a)
--     readWithFirstBase getFirst d text =
--       [ result
--         | res1@(v1, _) <- getFirst text,
--           res2@(op, _) <- lex (snd res1),
--           result <-
--             if isOp op
--               then [(applyOp op v1 t2, rest) | (t2, rest) <- (readsPrec::Int -> ReadS (Equation a)) d $ snd res2]
--               else [(v1, snd res1)]
--       ]

--     readTerm :: Int -> ReadS (Equation a)
--     readTerm d text = first Term <$> readValue d text
--     readValue :: Int -> ReadS a
--     readValue = undefined
--     -- readValue = readsPrec::Int -> ReadS a
--     isOp op = op == "+" || op == "*"
--     applyOp op v1 v2 | op == "+" = Sum v1 v2 | otherwise = Product v1 v2

unitTestEquation :: IO ()
unitTestEquation =
  callTest
    ( do
        print "test"
        let n1 = Term 1
            n2 = Term 2
            n3 = Term 3
            n4 = Term 4
            fn v1 v2 = Product v1 (Sum v2 n1)
            eqParser :: ReadS (Equation Int)
            eqParser = reads
            testSet =
              [ (n1, "1"),
                (n2, "2"),
                (Sum n1 n2, "1+2"),
                (Product n1 n2, "1*2"),
                -- multiple
                (Sum n1 (Sum n2 (Sum n3 n4)), "1+2+3+4"),
                (Product n1 (Product n2 (Product n3 n4)), "1*2*3*4"),
                -- associativity
                (Product (Sum n1 n2) n3, "(1+2)*3"),
                (Product n1 (Sum n2 n3), "1*(2+3)"),
                (Product (Sum n1 n2) (Sum n3 n4), "(1+2)*(3+4)"),
                (fn n2 (fn n2 (fn n2 (fn n2 n1))), "2*(2*(2*(2*(1+1)+1)+1)+1)")
              ]

        printBanner "Show test"
        traverse_
          ( \(eq, strEq) -> do
              let showEq = show eq
              assertIsEqual strEq showEq
              print $ "Pass " ++ strEq
          )
          testSet
        printBanner "Read test"
        traverse_
          ( \(eq, strEq) -> do
              let parsedEq = eqParser strEq
              assertIsEqual [(eq,"")] parsedEq
              print $ "Pass " ++ strEq
          )
          testSet
        testDone
    )
    "unitTestEquation"

testReadShow :: IO ()
testReadShow =
  callTest
    ( do
        let show1 = showsPrec 11 1
            show2 = showsPrec 11 2
            show3 = showsPrec 11 3
            showL = showList [1 .. 10]

            tree0 = MyNode (MyNode (MyLeaf 1) (MyLeaf 2)) (MyLeaf 3)

        print $ show1 "a" ++ "b"
        print $ show1 $ "a" ++ "b"
        print $ show1 . show2 . show3 $ "a"
        print $ show2 . show3 . show3 . showL $ "a"

        print $ showsPrec 0 (MyLeaf 1) ""
        print $ showsPrec 0 (MyLeaf $ Just 1) ""
        print $ showsPrec 1 (MyLeaf 1) ""
        print $ showsPrec 0 (MyLeaf 1) ""
        print $ showsPrec 0 tree0 ""
        print $ showsPrec 0 tree0 ""

        let n1 = Term 1
            n2 = Term 2
            fn v1 v2 = Product v1 (Sum v2 n1)
            eq1 = Sum n1 (Sum n1 n2)
            eq2 = Product n1 (Sum n1 n2)
            eq3 = Product n1 (Product n1 n2)
            eq4 = Product (Sum n2 n2) (Sum n1 n2)
            eq5 = fn n2 (fn n2 (fn n2 (fn n2 n1)))
            -- eqParser :: ReadS (Equation Int)
            -- eqParser = reads
        print $ showsPrec 0 n1 ""
        print $ showsPrec 0 n2 ""
        print $ shows (Sum n1 n2) ""
        print $ shows (Product n1 n2) ""
        print $ "eq1:" ++ shows eq1 ""
        print $ "eq2:" ++ shows eq2 ""
        print $ "eq3:" ++ shows eq3 ""
        print $ "eq4:" ++ shows eq4 ""
        print $ "eq5:" ++ shows eq5 ""

        printBanner "Read"
        -- traverse_
        --   (\eq -> print $ eq ++ "   " ++ show (eqParser eq)) [ "1",
        --       "1a",
        --       "1+2",
        --       "1+1a",
        --       "(1+2)",
        --       "(1+2)+1",
        --       "1+2+3",
        --       "1*2",
        --       "1*2*3",
        --       "1*(2+3)",
        --       show eq1,
        --       show eq2,
        --       show eq3,
        --       show eq4,
        --       show eq5,
        --       show eq5 ++ "a"
        --     ]

        unitTestEquation
        testDone
    )
    "testReadShow"
