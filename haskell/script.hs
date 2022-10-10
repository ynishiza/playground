-- import System.Random

import Control.Arrow
import Control.Monad
import Control.Applicative
main = do
  let message = "Hello"
  putStrLn message

-- main = callTest (do
--   testFold
--   -- testTraverse
--   testTraverse2
--   -- testTraverse
--   -- testCompositionMonad
--   -- testFunctionMonad
--   -- testRandom
--   -- testTypeSyntax
--   -- testCase
--   -- testDo
--   -- testData
--   -- testArrow
--   ) "main"

type TestType a = [a]
data TestType2 a = List [a] | Point (a,a)
data TestType3 a = List2 ([] a) | Point2 (a,a)

-- f(x) = (x+1)(x+3) - x
f :: Num a => a -> a
f = do
  y <- (+1)
  z <- (+3)
  (y*z+)

assert :: Bool -> ()
assert cond =
  if cond then () else error "Assertion failed"

assertIO :: Show a => Bool -> a -> IO ()
assertIO cond message = do
  print message
  unless cond undefined

testDone = print "done"

callTest :: IO () -> String -> IO ()
callTest x message = do
  print $ "start:" ++ message
  x
  print $ "end:" ++ message

testFold = callTest (do
  let printer :: Show a => a -> IO () -> IO ()
      printer x accum = putStr (show x) >> accum

  foldr printer (putStr "") [1..10]
  putStrLn ""
  foldl (flip printer) (putStr "") [1..10]

  print $ foldr (&&) True $ repeat False
  testDone) "testFold"

testTraverse2 = callTest (do
  traverse print [1..10] :: IO [()]
  sequence $ fmap print [1..10] :: IO [()]
  mapM print [1..10]
  testDone
  ) "testTraverse2"

testTraverse = callTest (do
  let f x = [0,x+2,2*x]
  let x = [1..3]
  
  let y0 = pure [] :: [[Int]]
  let doTraverseTest f x = 
        foldr (\x (step, history, res) -> 
          let v = f x
              -- res = f [b]
              -- v = f b
              -- next = f (b:[b]) = f [b]
              next = liftA2 (:) v res
          in (step+1, history ++ [("step=" ++ show step ++ ",x=" ++ show x ++ " v=" ++ show v ++ " next=" ++ show next)], next)) (0, [], pure []) x

  let callTraverseTest f x name = do
        print $ "start name=" ++ name
        print $ "x=" ++ show x
        print $ "steps=" ++ show (doTraverseTest f x)
        print $ "traverse=" ++ show (traverse f x)
        print $ "done name=" ++ name
        print ""


  callTraverseTest f [1..3] "list"

  let f = (\x -> if mod x 3 == 0 then Just x else Nothing)
  callTraverseTest f [0,3] "Just"
  callTraverseTest f [0,3,6] "Just"
  callTraverseTest f [0,3,6,7] "Just"

  let f = (\x -> if mod x 5 == 0 then Left x else Right x)
  callTraverseTest f [1] "Either"
  callTraverseTest f [1,2] "Either"
  callTraverseTest f [1,2,3,5,6] "Either"

  traverse print [1..10]
  testDone
  ) "testTraverse"


testCompositionMonad = callTest (do
  let f = (*2)
  let g = (*) . (+3)    -- f(x,y) = y(x+3)

  -- test g <*> f == f >>= flip g
  let x = g <*> f $ 2
  let y = (f >>= flip g) 2
  let z = (f <**> g) 2
  assertIO (x == y) x
  assertIO (x == z) x

  let x = g <*> f $ 5
  let y = (f >>= flip g) 5
  let z = (f <**> g) 5
  assertIO (x == y) x
  assertIO (x == z) x) "testCompositionMonad"


testFunctionMonad = callTest (do 
  print $ f 3
  testDone) "testFunctionMonad"

testArrow = callTest (do
  let f = (+4) >>> (*3)
  print $ f 1
  print $ f 2
  testDone
            ) "testArrow"

testData = callTest (do
  let x = List2 [1] 
  let y = Point2 (1,10)
  testDone
           ) "testData"

testDo = callTest (do
  print "Enter name"
  x <- getLine
  print $ "Hello " ++ x
  testDone
         ) "testDo"

testCase = callTest (do 
  let f x = case x of 
        0:_ -> "zero" 
        1:_ -> "one" 
        _ -> "other"
  let g x = case x of 
        [f]
          | f == 0 -> "0"
          | f == 1 -> "1"
        [_,g]
          | g == 0 -> "00"
          | g == 1 -> "01"
          | otherwise -> "NA g"
        _ -> show x
  print $ map (\x -> f [x]) [0..10]
  print $ map (\x -> f [-1, x]) [0..10]
  print $ map (\x -> g [x]) [0..10]
  print $ map (\x -> g [-1, x]) [0..10]
  testDone
           ) "testCase"

-- testRandom = callTest (do
--   let seed = 20
--   let gen = mkStdGen seed
--   let rg = uniformR (1::Int, 100)
--   let val1 = rg gen
--   let val2 = rg (snd val1)
--   print (fst val1, fst val2) ) "testRandom"
--   0 ->  "zero"
--   _ -> "other"


testPatterBind = let
  f x 
    | x == 1 = "one"
    | x == 2, x < 10 = "two"
    | let n = "other", n == "a" = n
    | otherwise = "other"
  g x y
    | n:_ <- x, m:_ <- y, n == m = show n
    | otherwise = "other"
    where k = 100; s = 2
          l = 10
  in g [1] [2]

testPatternMatch = let
  k@[n,_,_] = [1,2,3]
  x = 1
  y = n where n = 1
  in True


testTypeSyntax = callTest (do
  let ss :: (Eq (f a), Functor f) => f a -> f a -> Bool
      ss x y = x == y
  let f :: Num a => a -> a
      f x = 2 * x
  print $ ss [1] [2]
  print $ ss [1] [1]
  testDone
                 ) "testTypeSyntax"

