-- imprt System.Random

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Traversable
import StaticArrow
import TestArrow
import TestMonad
import TestUtils

data MyType a b
  = A
  | B b
  | C {v1 :: a}
  | a :<> b

main =
  callTest
    ( do
        putStrLn "Run all? (y/n)"
        response <- getChar
        if response == 'y'
          then runAll
          else do
            testMonadFix
            -- testMonadFail
            -- testStaticArrow
    )
    "main"

runAll = do
  -- test: Basic
  testBindByPatternMatch
  testTypeSyntax
  testCaseExpression
  testList
  -- testRandom

  -- test: Fold, Traversable
  testFold
  testTraverse
  testTraverse2

  -- test: Category, Arrow
  testArrow
  testArrowLoop

  -- test: Applicative,Monads
  testMonad
  testWrappedMonad
  testCompositionMonad
  testFunctorMonad
  testMonadFail
  testMonadTransform
  testStateMonad
  testStaticArrow

-- TEST TEMPLATE
testTemplate =
  callTest
    ( do
        let x = 1
        print "copy me"
        testDone
    )
    "testTemplate"

testList =
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

testFold =
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

testTraverse2 =
  callTest
    ( do
        traverse print [1 .. 10] :: IO [()]
        sequence $ fmap print [1 .. 10] :: IO [()]
        mapM print [1 .. 10]
        testDone
    )
    "testTraverse2"

testTraverse =
  callTest
    ( do
        let f x = [0, x + 2, 2 * x]
        let x = [1 .. 3]

        let y0 = pure [] :: [[Int]]
        let doTraverseTest f x =
              foldr
                ( \x (step, history, res) ->
                    let v = f x
                        -- res = f [b]
                        -- v = f b
                        -- next = f (b:[b]) = f [b]
                        next = liftA2 (:) v res
                     in (step + 1, history ++ [("step=" ++ show step ++ ",x=" ++ show x ++ " v=" ++ show v ++ " next=" ++ show next)], next)
                )
                (0, [], pure [])
                x

        let callTraverseTest f x name = do
              print $ "start name=" ++ name
              print $ "x=" ++ show x
              print $ "steps=" ++ show (doTraverseTest f x)
              print $ "traverse=" ++ show (traverse f x)
              print $ "done name=" ++ name
              print ""

        callTraverseTest f [1 .. 3] "list"

        let f = (\x -> if mod x 3 == 0 then Just x else Nothing)
        callTraverseTest f [0, 3] "Just"
        callTraverseTest f [0, 3, 6] "Just"
        callTraverseTest f [0, 3, 6, 7] "Just"

        let f = (\x -> if mod x 5 == 0 then Left x else Right x)
        callTraverseTest f [1] "Either"
        callTraverseTest f [1, 2] "Either"
        callTraverseTest f [1, 2, 3, 5, 6] "Either"

        traverse print [1 .. 10]
        testDone
    )
    "testTraverse"

testDoExpression =
  callTest
    ( do
        print "Enter name"
        x <- getLine
        print $ "Hello " ++ x
        testDone
    )
    "testDoExpression"

testCaseExpression =
  callTest
    ( do
        let f x = case x of
              0 : _ -> "zero"
              1 : _ -> "one"
              _ -> "other"
        let g x = case x of
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

testBindByPatternMatch =
  callTest
    ( do
        let f x
              | x == 1 = "one"
              | x == 2, x < 10 = "two"
              | let n = "other", n == "a" = n
              | otherwise = "other"
        let g x y
              | n : _ <- x, m : _ <- y, n == m = "x[0] == y[0] == " ++ show n
              | otherwise = "other"
              where
                k = 100
                s = 2
                l = 10

        printBanner "f"
        print $ f 1
        print $ f 2
        print $ f 3

        printBanner "g"
        print $ g [1] [1]
        print $ g [1] [2]
        testDone
    )
    "testBindByPatternMatch"

testPatternMatch =
  let k@[n, _, _] = [1, 2, 3]
      x = 1
      y = n where n = 1
   in True

testTypeSyntax =
  callTest
    ( do
        let ss :: (Eq (f a), Functor f) => f a -> f a -> Bool
            ss x y = x == y
        let f :: Num a => a -> a
            f x = 2 * x
        print $ ss [1] [2]
        print $ ss [1] [1]
        testDone
    )
    "testTypeSyntax"
