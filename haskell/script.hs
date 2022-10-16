-- import System.Random
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Const
import Data.Foldable
import Data.Traversable
import Data.List
import Data.Function
import Data.Char
import Control.Arrow
import Control.Monad
import Control.Applicative

main = callTest (do
  -- testPatterBind
  -- testWrapped
  -- testFunctorMonad
  -- testFold
  -- testMonad
  -- testTraverse
  -- testTraverse2
  -- testTraverse
  -- testCompositionMonad
  -- testFunctionMonad
  -- testRandom
  -- testTypeSyntax
  -- testCase
  -- testDoExpression
  -- testData
  testArrow
  ) "main"

assert :: Bool -> ()
assert cond =
  if cond then () else error "Assertion failed"

assertIO :: Show a => Bool -> a -> IO ()
assertIO cond message = do
  print message
  unless cond undefined

testDone = putStrLn "done"

printBanner msg = putStrLn $ "=====" ++ msg ++ "====="

printList :: (Foldable f, Show a) => f a -> IO ()
printList = traverse_ print 

callTest :: IO () -> String -> IO ()
callTest x message = do
  putStrLn $ "start:" ++ message
  x
  putStrLn $ "end:" ++ message

-- TEST TEMPLATE
testTemplate = callTest (do
  let x = 1
  print "copy me"
  testDone) "testTemplate"

testList = callTest (do
  print $ uncons [1]
  print $ uncons ([]::[Int])
  let x:xs = [1,2]
  let Just (x,xs) = uncons $ x:xs

  print $ find (==2) [2..10]
  print $ lookup 1 [(1,2),(2,3)]
  print $ lookup 2 [(1,2),(1,3)]
  print $ partition (<2) [1..10]
  print $ intersperse 5 [10..20]
  print $ transpose [[1,2,3],[4..6]]
  print $ transpose [[1,2,3],[4..7]]

  print $ subsequences [1..3]
  testDone) "testList"

testFunctorMonad = callTest (do
  let a = Identity 1
  let l = InL $ Just 1
  let r = InR $ Just 2 

  let printSum = traverse_ print
  printSum l
  printSum r
  testDone) "testFunctionMonad"

testMonad = callTest (do
  do 
    putStr "Hello" 
    putStr "there" 

  do 
    putStr "Enter value"
    x <- getLine
    let msg = "message" ++ x
    putStr msg

  let f = do
        t <- (+10)
        (+(2*t))
  print $ f 3
  let x = do
        t <- [1..10]
        [1,2..t]
  print x

  testDone
  ) "testMonad"

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
  let f = (*2)
  print $ f 3
  testDone) "testFunctionMonad"

testArrow = callTest (do
  -- Basic
  let f = (+4) >>> (*3)
  print $ f 1
  print $ f 2

  -- Conditional
  let f :: Either Int Char -> Either Int Char
      f = left (*3)
  let g :: Either Int Char -> Either Int Char
      g = right $ chr . (+1) .ord
  print $ f $ Left 1
  print $ f $ Right 'a'
  print $ g $ Left 1
  print $ g $ Right 'a'

  let f = (+2) +++ (+3)
  print $ f $ Left 1
  print $ f $ Right 1
  let f = (+2) ||| ord

  print $ f $ Left 1
  print $ f $ Right 'a'
  testDone
            ) "testArrow"

testArrowLoop = callTest (do
  -- loop 
  printBanner "Loop test"

  -- an equivalent implementation of loop
  let loop2 f x = let
          f1 = fst.f
          f2 = snd.f
        in f1 (x, fix (\b -> f2 (x,b)))


  let factorialFix fact n = if n <= 1 then n else n * fact (n-1)
  let factorialLoop (n, f) = (f n, factorialFix f)
  let factorialLoop2 (n, f) = (f n 1, g f)
        where g f i accum = if i <= 0 then accum else f (i-1) (i * accum)

  let fibonacchiFix fib n = if n <= 1 then n else fib (n-1) + fib (n-2)
  let fibonacci (n, f) = (f n, fibonacchiFix f)

  let testLoop f testName = do
          printBanner $ "start:" ++ testName
          printList $ f <$> [1..10]
          printBanner $ "done:" ++ testName

  testLoop (loop factorialLoop2) "loop+factorialLoop2"
  testLoop (loop2 factorialLoop2) "loop2+factorialLoop2"

  testLoop (fix factorialFix) "fix+factorialFix"
  testLoop (loop factorialLoop) "loop+factorialLoop"
  testLoop (loop2 factorialLoop) "loop2+factorialLoop"

  testLoop (fix fibonacchiFix) "fix+fibonacci0"
  testLoop (loop fibonacci) "loop+fibonacci"
  testLoop (loop2 fibonacci) "loop2+fibonacci"
  testDone
            ) "testArrowLoop"


data TestType3 a = List2 ([] a) | Point2 (a,a)
testData = callTest (do
  let x = List2 [1] 
  let y = Point2 (1,10)
  testDone
           ) "testData"


testDoExpression = callTest (do
  print "Enter name"
  x <- getLine
  print $ "Hello " ++ x
  testDone
         ) "testDoExpression"

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


testPatterBind = callTest (do
  let f x
        | x == 1 = "one"
        | x == 2, x < 10 = "two"
        | let n = "other", n == "a" = n
        | otherwise = "other"
  let g x y
        | n:_ <- x, m:_ <- y, n == m = "x[0] == y[0] == " ++ show n
        | otherwise = "other"
        where k = 100; s = 2
              l = 10

  printBanner "f"
  print $ f 1
  print $ f 2
  print $ f 3

  printBanner "g"
  print $ g [1] [1]
  print $ g [1] [2]
  testDone) "testPatterBind"


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

testWrapped = callTest (do
  let x = (WrapMonad $ Just 1) :: WrappedMonad Maybe Int
  let x = WrapMonad [1] :: WrappedMonad [] Int
  let f = WrapArrow (*2)
  print $ (unwrapArrow $ (+) <$> f <*> f) 2
  print $ unwrapMonad $ (*2) <$> x
  testDone) "testWrapped"
