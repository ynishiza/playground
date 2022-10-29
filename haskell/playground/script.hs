{-# HLINT ignore "Use mapM_" #-}
{-# HLINT ignore "Use mapM" #-}
{-# HLINT ignore "Use and" #-}
-- import System.Random
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use shows" #-}
{-# HLINT ignore "Use show" #-}
{- ORMOLU_DISABLE -}
import Debug.Trace (trace)
import Control.Applicative
import Control.Monad
import Control.Arrow (first)
import Data.Int
import Data.List (uncons, partition, intersperse, subsequences, transpose)
import Data.Fixed (Deci, Fixed (..), Uni, resolution)
import Data.Foldable
import qualified TestArrow
import qualified TestMonad
import qualified TestModuleTransformer
import qualified TestModuleMtl
import qualified TestMyStateMonad
import qualified TestStateMonadExample
import qualified TestTypeClass
{- ORMOLU_ENABLE -}
import TestUtils

infinity = (1 / 0) :: Double

main :: IO ()
main =
  callTest
    ( do
        putStrLn "Run all? (y/n)"
        response <- getChar
        if response == 'y'
          then runAll
          else do
            testReadShow
            -- TestTypeClass.testDerivedInstance
            -- TestModuleMtl.testMyIOState
            -- TestMyStateMonad.testStateMonad
            -- TestStateMonadExample.runTest
            -- TestMonad.testMonadFix
            -- TestModuleTransformer.testLazyStateMonad
    )
    "main"

runAll :: IO ()
runAll = do
  runAllLocal

  -- test: Category, Arrow
  TestArrow.runAll

  -- test: Applicative,Monads
  TestMonad.runAll
  TestModuleTransformer.runAll
  TestModuleMtl.testMyIOState

  TestMyStateMonad.testStateMonad
  TestStateMonadExample.runTest

  TestTypeClass.runAll

runAllLocal :: IO ()
runAllLocal =
  callTest
    ( do
        testFixed
        testListFunctions
        testFoldable
        testTraversableInSteps
        testTraversable
        testCaseExpression
        testBindByPatternMatch
        testTypeSyntax
        testNumericalConversion
    )
    "basic"

-- testRandom

-- TEST TEMPLATE
testTemplate :: IO ()
testTemplate =
  callTest
    ( do
        let x = 1
        print "copy me"
        testDone
    )
    "testTemplate"

testFixed :: IO ()
testFixed =
  callTest
    ( do
        let x = MkFixed 10

        print $ let (MkFixed y) = x in y
        print $ resolution (x :: Uni)
        print $ resolution (x :: Deci)
        testDone
    )
    "testFixed"

testListFunctions :: IO ()
testListFunctions =
  callTest
    ( do
        print $ uncons [1]
        print $ uncons ([] :: [Int])
        let x : xs = [1, 2]
        let Just (x, xs) = uncons $ x : xs

        print $ find (== 2) [2 .. 10]
        print $ lookup 1 [(1, 2), (2, 3)]
        print $ lookup 2 [(1, 2), (1, 3)]
        print $ partition (< 2) [1 .. 10]
        print $ intersperse 5 [10 .. 20]
        print $ transpose [[1, 2, 3], [4 .. 6]]
        print $ transpose [[1, 2, 3], [4 .. 7]]

        print $ subsequences [1 .. 3]
        testDone
    )
    "testList"

testFoldable :: IO ()
testFoldable =
  callTest
    ( do
        let printer :: Show a => a -> IO () -> IO ()
            printer x accum = putStr (show x) >> accum

        foldr printer (putStr "") [1 .. 10]
        putStrLn ""
        foldl (flip printer) (putStr "") [1 .. 10]

        print $ foldr (&&) True $ repeat False
        testDone
    )
    "testFold"

testTraversable :: IO ()
testTraversable =
  callTest
    ( do
        traverse print [1 .. 10] :: IO [()]
        sequence $ fmap print [1 .. 10] :: IO [()]
        mapM print [1 .. 10]
        testDone
    )
    "testTraversable"

type TraverseAction a f b = (a -> f b)

type GetTraverseStepInfo a f b = Int -> a -> f b -> f [b] -> String

type TraverseStepResult f b = (Int, [Message], f [b])

testTraversableInSteps :: IO ()
testTraversableInSteps =
  callTest
    ( do
        let f :: Int -> [Int]
            f x = [0, x + 2, 2 * x]
            x = [1 .. 3]

            y0 = pure [] :: [[Int]]

            getStepInfo :: (Show a, Show (f b), Show (f [b])) => GetTraverseStepInfo a f b
            getStepInfo step x v next =
              "step=" ++ show step ++ ",x=" ++ show x ++ " v=" ++ show v ++ " next=" ++ show next
            showTraverseSteps ::
              (Traversable t, Applicative f) =>
              TraverseAction a f b ->
              GetTraverseStepInfo a f b ->
              t a ->
              TraverseStepResult f b
            showTraverseSteps f getInfo =
              foldr
                ( \x (step, history, res) ->
                    let v = f x
                        next = liftA2 (:) v res
                        msg = getInfo step x v next
                     in (step + 1, history ++ [msg], next)
                )
                (0, [], pure [])

            showTraverseStepsFor ::
              (Traversable t, Applicative f, Show (f [b]), Show (t a), Show (f (t b))) =>
              Name ->
              TraverseAction a f b ->
              GetTraverseStepInfo a f b ->
              t a ->
              IO ()
            showTraverseStepsFor name f getInfo x = do
              print $ "start name=" ++ name
              print $ "x=" ++ show x
              print $ "steps=" ++ show (showTraverseSteps f getInfo x)
              print $ "traverse=" ++ show (traverse f x)
              print $ "done name=" ++ name
              print ""

        showTraverseStepsFor "list" f getStepInfo [1 .. 3]

        let f x = if mod x 3 == 0 then Just x else Nothing
        showTraverseStepsFor "Just" f getStepInfo [0 :: Int, 3]
        showTraverseStepsFor "Just" f getStepInfo [0 :: Int, 3, 6]
        showTraverseStepsFor "Just" f getStepInfo [0 :: Int, 3, 6, 7]

        let f x = if mod x 5 == 0 then Left x else Right x
        showTraverseStepsFor "Either" f getStepInfo [1 :: Int]
        showTraverseStepsFor "Either" f getStepInfo [1 :: Int, 2]
        showTraverseStepsFor "Either" f getStepInfo [1 :: Int, 2, 3, 5, 6]

        traverse_ print [1 .. 10]
        testDone
    )
    "testTraversableInSteps"

testCaseExpression :: IO ()
testCaseExpression =
  callTest
    ( do
        let f x = case x of
              0 : _ -> "zero"
              1 : _ -> "one"
              _ -> "other"
            g x = case x of
              [f]
                | f == 0 -> "0"
                | f == 1 -> "1"
              [_, g]
                | g == 0 -> "00"
                | g == 1 -> "01"
                | otherwise -> "NA g"
              _ -> show x
        print $ map (\x -> f [x]) [0 .. 10]
        print $ map (\x -> f [-1, x]) [0 .. 10]
        print $ map (\x -> g [x]) [0 .. 10]
        print $ map (\x -> g [-1, x]) [0 .. 10]
        testDone
    )
    "testCaseExpression"

-- testRandom = callTest (do
--   let seed = 20
--   let gen = mkStdGen seed
--   let rg = uniformR (1::Int, 100)
--   let val1 = rg gen
--   let val2 = rg (snd val1)
--   print (fst val1, fst val2) ) "testRandom"
--   0 ->  "zero"
--   _ -> "other"

testBindByPatternMatch :: IO ()
testBindByPatternMatch =
  callTest
    ( do
        let f x
              | x == 1 = "one"
              | x == 2, x < 10 = "two"
              | let n = "other", n == "a" = n
              | otherwise = "other"
            g x y
              | n : _ <- x, m : _ <- y, n == m = "x[0] == y[0] == " ++ show n
              | otherwise = "other"
              where
                k = 100 :: Int
                s = 2
                l = 10

        printBanner "f"
        print $ f 1
        print $ f 2
        print $ f 3

        printBanner "g"
        print $ g [1 :: Int] [1 :: Int]
        print $ g [1 :: Int] [2 :: Int]
        testDone
    )
    "testBindByPatternMatch"

testPatternMatch =
  let k@[n, _, _] = [1, 2, 3]
      x = 1
      y = n where n = 1
   in True

testTypeSyntax :: IO ()
testTypeSyntax =
  callTest
    ( do
        let ss :: (Eq (f a), Functor f) => f a -> f a -> Bool
            ss x y = x == y
            f :: Num a => a -> a
            f x = 2 * x
        print $ ss [1 :: Int] [2 :: Int]
        print $ ss [1 :: Int] [1 :: Int]
        testDone
    )
    "testTypeSyntax"

testNumericalConversion :: IO ()
testNumericalConversion =
  callTest
    ( do
        let vint = 1 :: Int
            vinteger = 1 :: Integer
            vint8 = 1 :: Int8
            vdouble = 1.1 :: Double
            vfloat = 1.1 :: Float

        -- integer -> real number
        print $ fromIntegral vint + vdouble
        print $ fromIntegral vint + vfloat
        print $ fromIntegral vint8 + vdouble
        print $ fromIntegral vinteger + vdouble

        -- real number -> integer
        print $ vint + round vdouble
        print $ vint + round vfloat
        print $ vinteger + round vdouble
        print $ vinteger + round vdouble

        -- coerce different integer types
        print $ fromIntegral vint + vint8
        print $ vint + fromIntegral vint8

        -- case: Integer type is a special case because it is unbounded
        -- bounded to unbounded
        print $ toInteger vint + vinteger
        print $ toInteger vint8 + vinteger
        -- unbounded to bounded
        print $ vint + fromInteger vinteger
        print $ vint8 + fromInteger vinteger

        -- coerce real numbers
        print $ vfloat + realToFrac vdouble
        print $ realToFrac vfloat + vdouble

        print $ toInteger (1 :: Int) + (2 :: Integer)
        testDone
    )
    "testNumericalConversion"

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
  -- readsPrec d text = readLeftmostTerm d text
    where
      readWithoutBrackets _d _text = readLeftmostTerm _d _text ++ readLeftmostEq _d _text
      readLeftmostTerm _d = readLeftmost (readOneTerm _d) _d
      readLeftmostEq _d = readLeftmost (readParenAlways (readsPrec _d)) _d

      readLeftmost :: ReadS (Equation a) -> ReadsPrec (Equation a)
      -- readLeftmost = undefined
      readLeftmost readLeft _d _text = do
        (leftEq, leftRest) <- readLeft _text
        if null leftRest
          -- then trace ("no right" ++ leftRest) $ return (leftEq, leftRest)
          -- else trace ("right" ++ leftRest) $ readRight leftEq _d leftRest
          then return (leftEq, leftRest)
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
          ++ 
            do
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
              let parsedEq = fst . head . eqParser $ strEq
              assertIsEqual eq parsedEq
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
            n3 = Term 3
            n4 = Term 4
            fn v1 v2 = Product v1 (Sum v2 n1)
            eq1 = Sum n1 (Sum n1 n2)
            eq2 = Product n1 (Sum n1 n2)
            eq3 = Product n1 (Product n1 n2)
            eq4 = Product (Sum n2 n2) (Sum n1 n2)
            eq5 = fn n2 (fn n2 (fn n2 (fn n2 n1)))
            eqParser :: ReadS (Equation Int)
            eqParser = reads
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
