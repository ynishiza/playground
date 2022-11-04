{-# HLINT ignore "Use mapM_" #-}
{-# HLINT ignore "Use mapM" #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Use shows" #-}
{-# HLINT ignore "Use show" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestBase
  ( allTests,
  )
where

{- ORMOLU_DISABLE -}
import System.Random (mkStdGen, uniformR)
import Control.Applicative
import Data.Int
import Data.List (uncons, partition, intersperse, subsequences, transpose)
import Data.Fixed (Deci, Fixed (..), Uni, resolution)
import Data.Foldable
import qualified TestArrow
import qualified TestMonad
import qualified Transformer.TestBase as Transformer
import qualified Mtl.TestBase as Mtl
import qualified TestTypeClass
import qualified TestReadShow
{- ORMOLU_ENABLE -}
import TestUtils

-- infinity :: Double
-- infinity = (1 / 0) :: Double

allTests :: TestState
allTests =
  wrapTest
    ( do
        runAllLocal

        -- test: Category, Arrow
        TestArrow.allTests

        -- test: Applicative,Monads
        TestMonad.allTests
        Transformer.allTests
        Mtl.allTests

        TestTypeClass.allTests
        TestReadShow.allTests
    )
    ""

runAllLocal :: TestState
runAllLocal =
  wrapTest
    ( do
        testFixed
        testListUtilityFunctions
        testFoldable
        testTraversableInSteps
        testTraversable
        testCaseExpression
        testBindGuards
        testTypeSignature
        testNumericalConversion
        testRandom
    )
    "basic"

testFixed :: TestState
testFixed =
  createTest
    ( do
        let x = MkFixed 10

        print $ let (MkFixed y) = x in y
        print $ resolution (x :: Uni)
        print $ resolution (x :: Deci)
        testDone
    )
    "testFixed"

testListUtilityFunctions :: TestState
testListUtilityFunctions =
  createTest
    ( do
        print $ uncons [1]
        print $ uncons ([] :: [Int])
        -- let x : _ = [1, 2]
        -- let Just (x, xs) = uncons $ x : xs

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

testFoldable :: TestState
testFoldable =
  createTest
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

testTraversable :: TestState
testTraversable =
  createTest
    ( do
        _ <- traverse print [1 .. 10] :: IO [()]
        _ <- sequence $ fmap print [1 .. 10] :: IO [()]
        _ <- mapM print [1 .. 10]
        testDone
    )
    "testTraversable"

type TraverseAction a f b = (a -> f b)

type GetTraverseStepInfo a f b = Int -> a -> f b -> f [b] -> String

type TraverseStepResult f b = (Int, [Message], f [b])

testTraversableInSteps :: TestState
testTraversableInSteps =
  createTest
    ( do
        let f :: Int -> [Int]
            f x = [0, x + 2, 2 * x]
            -- x = [1 .. 3]

            -- y0 = pure [] :: [[Int]]

            getStepInfo :: (Show a, Show (f b), Show (f [b])) => GetTraverseStepInfo a f b
            getStepInfo step x v next =
              "step=" ++ show step ++ ",x=" ++ show x ++ " v=" ++ show v ++ " next=" ++ show next
            showTraverseSteps ::
              (Traversable t, Applicative f) =>
              TraverseAction a f b ->
              GetTraverseStepInfo a f b ->
              t a ->
              TraverseStepResult f b
            showTraverseSteps g getInfo =
              foldr
                ( \x (step, history, res) ->
                    let v = g x
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
            showTraverseStepsFor name fn getInfo x = do
              print $ "start name=" ++ name
              print $ "x=" ++ show x
              print $ "steps=" ++ show (showTraverseSteps fn getInfo x)
              print $ "traverse=" ++ show (traverse fn x)
              print $ "done name=" ++ name
              print ""

        showTraverseStepsFor "list" f getStepInfo [1 .. 3]

        let f2 x = if mod x 3 == 0 then Just x else Nothing
        showTraverseStepsFor "Just" f2 getStepInfo [0 :: Int, 3]
        showTraverseStepsFor "Just" f2 getStepInfo [0 :: Int, 3, 6]
        showTraverseStepsFor "Just" f2 getStepInfo [0 :: Int, 3, 6, 7]

        let f3 x = if mod x 5 == 0 then Left x else Right x
        showTraverseStepsFor "Either" f3 getStepInfo [1 :: Int]
        showTraverseStepsFor "Either" f3 getStepInfo [1 :: Int, 2]
        showTraverseStepsFor "Either" f3 getStepInfo [1 :: Int, 2, 3, 5, 6]

        traverse_ print [1 .. 10]
        testDone
    )
    "testTraversableInSteps"

testCaseExpression :: TestState
testCaseExpression =
  createTest
    ( do
        let f x = case x of
              0 : _ -> "zero"
              1 : _ -> "one"
              _ -> "other"
            g x = case x of
              [y]
                | y == 0 -> "0"
                | y == 1 -> "1"
              [_, ys]
                | ys == 0 -> "00"
                | ys == 1 -> "01"
                | otherwise -> "NA g"
              _ -> show x
        print $ map (\x -> f [x]) [0 .. 10]
        print $ map (\x -> f [-1, x]) [0 .. 10]
        print $ map (\x -> g [x]) [0 .. 10]
        print $ map (\x -> g [-1, x]) [0 .. 10]
        testDone
    )
    "testCaseExpression"

testRandom :: TestState
testRandom =
  createTest
    ( do
        let seed = 20
        let gen = mkStdGen seed
        let rg = uniformR (1 :: Int, 100)
        let val1 = rg gen
        let val2 = rg (snd val1)
        print (fst val1, fst val2)
    )
    "testRandom"

testBindGuards :: TestState
testBindGuards =
  createTest
    ( do
        let f x
              | x == 1 = "one"
              | x == 2, x < 10 = "two"
              | let n = "other", n == "a" = n
              | otherwise = "other"
            g x y
              | n : _ <- x, m : _ <- y, n == m = "x[0] == y[0] == " ++ show n
              | otherwise = "other"
        -- where
        -- k = 100 :: Int
        -- s = 2
        -- l = 10

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

testPatternMatch :: [Int]
testPatternMatch =
  let k@[_, _, _] = [1, 2, 3]
      _ = 1
      _ = m where m = 1
   in k

testTypeSignature :: TestState
testTypeSignature =
  createTest
    ( do
        let ss :: (Eq (f a), Applicative f, Eq a) => f a -> f a -> (Bool, f Bool)
            ss x y = (x == y, (==) <$> x <*> y)
            f :: Num a => a -> a
            f x = 2 * x
        print $ ss [1 :: Int] [2 :: Int]
        print $ ss [1 :: Int] [1 :: Int]
        print $ f 3
        testDone
    )
    "testTypeSyntax"

testNumericalConversion :: TestState
testNumericalConversion =
  createTest
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
