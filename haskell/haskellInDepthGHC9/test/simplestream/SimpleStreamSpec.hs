{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Alternative law, right identity" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use map once" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module SimpleStreamSpec
  ( spec,
  )
where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.Function
import Data.Functor.Compose
import Data.Functor.Sum
import Data.List (isInfixOf)
import Data.Time.Clock.POSIX
import Fmt
import SimpleStream.Extra
import SimpleStream.Prelude
  ( Of (..),
    StreamOf,
    each,
    toList,
    toListEffect_,
    toListReturn_,
    toList_,
    yield,
  )
import SimpleStream.Prelude qualified as S
import SimpleStream.Stream
import System.IO.Extra
import System.Random
import Test.Hspec

emptyInt :: StreamOf Int m ()
emptyInt = empty_

emptyEffect :: (Monad m, Functor f) => Stream f m ()
emptyEffect = Effect $ return $ pure ()

oneSecond :: Int
oneSecond = 1 * 1000 * 1000

withTrivialEffect :: (Monad m, Functor f) => Stream f m r -> Stream f m r
withTrivialEffect = mapped pure

eachForTest :: Monad m => [a] -> StreamOf a m ()
eachForTest v = withTrivialEffect $ each v

testStream :: (Show a, Eq a, Show r, Eq r) => ([a], r) -> StreamOf a IO r -> IO ()
testStream (expected, r) str = toList str >>= (`shouldBe` (expected :> r))

getTimestamp :: IO Double
getTimestamp = fromRational . toRational <$> getPOSIXTime

allInRange :: Ord e => e -> e -> [e] -> Bool
allInRange x y = all (inRange x y)

inRange :: Ord e => e -> e -> e -> Bool
inRange x y v = x <= v && v <= y

testStreamWithBase_ :: (Show a, Eq a) => [a] -> StreamOf a IO () -> IO ()
testStreamWithBase_ expected str = do
  testStream (expected, ()) str
  testStream (expected, ()) $ withTrivialEffect str

testStreamWithBase_' :: (Show a, Eq a) => StreamOf a IO () -> [a] -> IO ()
testStreamWithBase_' = flip testStreamWithBase_

randomList :: (Uniform a, MonadIO m) => Int -> m [a]
randomList n = replicateM n (getStdRandom uniform)

printEffect :: Show a => a -> IO a
printEffect x = print x >> return x

type IOWriter a = WriterT a IO

toListAndPrint :: (MonadIO m, Buildable [a]) => StreamOf a m r -> m (Of [a] r)
toListAndPrint s = do
  (v :> r) <- S.toList s
  liftIO $ fmt $ "[" +| v |+ "]"
  return (v :> r)

spec :: SpecWith ()
spec = describe "Simple Stream" $ do
  baseSpec
  preludeSpec
  extraSpec

baseSpec :: SpecWith ()
baseSpec = describe "Stream" $ do
  describe "Basics" $ do
    it "creates a stream with steps" $ do
      let s = do
            yield 1
            yield 2
      s `testStreamWithBase_'` [1, 2 :: Int]

    it "creates a stream with effects" $ do
      startTime <- getTimestamp
      let s = do
            yield 1
            Effect $ do
              threadDelay oneSecond
              eachForTest <$> replicateM 2 getTimestamp
            Effect $ do
              eachForTest <$> replicateM 3 getTimestamp
            yield 4
      (res :> _) <- toList s
      endTime <- getTimestamp

      length res `shouldBe` 7
      head res `shouldBe` 1
      last res `shouldBe` 4
      drop 1 (take 5 res) `shouldSatisfy` allInRange startTime endTime

    it "joins a stream" $ do
      let s1 = do
            yield 1
            yield 2
          s2 = do
            yield 3
            s1
            yield 10
      s2 `testStreamWithBase_'` [3, 1, 2, 10 :: Int]

    it "[Monoid] sum" $ do
      let s1 = eachForTest ["a", "b", "c"]
          s2 = eachForTest ["w", "x", "y", "z"]
      (s1 <|> s2) `testStreamWithBase_'` ["aw", "bx", "cy" :: String]
      (s2 <|> s1) `testStreamWithBase_'` ["wa", "xb", "yc"]

    -- Laws
    it "[Monoid] empty law empty <|> x == x <|> empty == x" $ do
      let s1 = eachForTest ["a", "b", "c"]
      ( empty <|> s1
        )
        `testStreamWithBase_'` ["a", "b", "c" :: String]
      ( s1 <|> empty
        )
        `testStreamWithBase_'` ["a", "b", "c"]

  describe "Constructing streams" $ do
    it "[replicates]" $ do
      replicates 4 ('a' :> ())
        `testStreamWithBase_'` "aaaa"

    it "[repeats]" $ do
      let s = repeats ('a' :> ())
      takes 2 s `testStreamWithBase_'` "aa"
      takes 4 s `testStreamWithBase_'` "aaaa"

    it "[repeatsM]" $ do
      let s = repeatsM $ return ('a' :> ())
      takes 4 s `testStreamWithBase_'` "aaaa"
      takes 2 s `testStreamWithBase_'` "aa"

    it "[unfolds]" $ do
      unfold (\x -> return $ if x < 5 then Right (x * 10 :> x + 1) else Left ()) 0
        `testStreamWithBase_'` [0, 10, 20, 30, 40 :: Int]

    it "[untilJust]" $ do
      let s :: StreamOf String (State String) Int
          s = untilJust $ do
            v <- gets length
            if v > 5
              then return (Just 1000)
              else modify' (++ "a") >> return Nothing

          (elems :> result, logData) =
            flip runState "" $ toList s

      elems `shouldBe` replicate 6 mempty
      result `shouldBe` 1000
      logData `shouldBe` "aaaaaa"
      length elems `shouldBe` length logData

    it "[delays]" $ do
      let t = round @Double $ fromIntegral oneSecond / 2

      startTime <- getTimestamp
      x <-
        eachForTest [1 .. 5 :: Int]
          & zipPair (delays @IO @(Of String) t)
          & S.toList_
      endTime <- getTimestamp
      x `shouldBe` [("", 1), ("", 2), ("", 3), ("", 4), ("", 5)]
      print $ endTime - startTime
      endTime - startTime `shouldSatisfy` (>= 2.5)
      endTime - startTime `shouldSatisfy` (<= 3.5)

  describe "Transforming streams" $ do
    it "[maps]" $ do
      ( eachForTest [1 .. 4 :: Int]
          & maps (S.mapOf (10 *))
          & maps (S.mapOf show)
        )
        `testStreamWithBase_'` ["10", "20", "30", "40"]

    it "[mapsM]" $ do
      ( eachForTest [1 .. 4 :: Int]
          & mapsM (pure . S.mapOf (10 *))
          & mapsM (pure . S.mapOf show)
        )
        `testStreamWithBase_'` ["10", "20", "30", "40"]

    it "[groups]" $ do
      let s :: Stream (Sum (Of Int) (Of Int)) IO ()
          s = do
            yields $ InL (1 :> ())
            yields $ InL (2 :> ())
            yields $ InR (1 :> ())
            yields $ InR (2 :> ())
            yields $ InL (3 :> ())
            yields $ InR (1 :> ())
            yields $ InL (2 :> ())
          f =
            withTrivialEffect
              >>> groups
              >>> maps
                ( \case
                    (InL str) ->
                      str
                        & S.map (* 2)
                        & S.map (+ 1)
                        & S.map show
                        & S.map ("L:" ++)
                    (InR str) ->
                      str
                        & S.map (* 100)
                        & S.map negate
                        & S.map show
                        & S.map ("R:" ++)
                )
              >>> mapped toList
      f s
        `testStreamWithBase_'` [ ["L:3"],
                                 ["L:5"],
                                 ["R:-100"],
                                 ["R:-200"],
                                 ["L:7"],
                                 ["R:-100"],
                                 ["L:5"]
                               ]

  describe "Inspecting streams" $ do
    it "[inspects]" $ do
      (Right (x1 :> s1)) <- inspect $ eachForTest [1, 2, 3 :: Int]
      (Right (x2 :> s2)) <- inspect s1
      (Right (x3 :> s3)) <- inspect s2
      (Left r) <- inspect s3
      x1 `shouldBe` 1
      x2 `shouldBe` 2
      x3 `shouldBe` 3
      r `shouldBe` ()

  describe "Splitting and joining streams" $ do
    describe "[splitsAt]" $ do
      it "splits the stream" $ do
        (x, y) <-
          eachForTest [1 .. 5 :: Int]
            & splitsAt 2
            & toListReturn_
        x `shouldBe` [1, 2]
        y `shouldBe` [3, 4, 5]

      it "preserves effect" $ do
        startTime <- getTimestamp
        (x, y) <-
          eachForTest [1 .. 5 :: Int]
            & S.map (* 10)
            & S.mapM (\a -> (a,) <$> getTimestamp)
            & splitsAt 2
            & toListReturn_
        endTime <- getTimestamp

        fst <$> x `shouldBe` [10, 20]
        snd <$> x `shouldSatisfy` allInRange startTime endTime
        fst <$> y `shouldBe` [30, 40, 50]

      it "splits the stream at 0" $ do
        (x, y) <-
          eachForTest [1 .. 5 :: Int]
            & splitsAt 0
            & toListReturn_
        x `shouldBe` []
        y `shouldBe` [1 .. 5]

      it "splits the stream longer" $ do
        (x, y) <-
          eachForTest [1 .. 5 :: Int]
            & splitsAt 100
            & toListReturn_
        x `shouldBe` [1 .. 5]
        y `shouldBe` []

    let extractChunk :: forall a m r. Monad m => ChunkedStream (Of a) m r -> m ([[a]], r)
        extractChunk =
          collects
            ( \s -> do
                (x :> r) <- toList s
                return ([x], r)
            )
    describe "[chunks, concats]" $ do
      it "splits the stream into chunks" $ do
        let s = eachForTest [1 .. 10 :: Int]
        (r, _) <- extractChunk $ chunks 3 s
        r
          `shouldBe` [ [1, 2, 3],
                       [4, 5, 6],
                       [7, 8, 9],
                       [10]
                     ]

      it "[concats]" $ do
        let chunkedStream = chunks 3 $ eachForTest [1 .. 10 :: Int]
        concats chunkedStream `testStreamWithBase_'` [1 .. 10]

      it "concats and chunks are inverses" $ do
        l <- randomList @Int 10
        let stream = eachForTest l
        ( chunks 3 stream
            & concats
          )
          `testStreamWithBase_'` l

      it "throws an error if the chunk size is not positive" $ do
        let s = eachForTest @IO [1 .. 4 :: Int]
        evaluate (chunks 0 s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)
        evaluate (chunks (-10) s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)

    it "[joins]" $ do
      let innerStream = eachForTest ['a' .. 'd']
          stream = do
            yield 'A'
            yield 'X'
            return innerStream
      joins stream `testStreamWithBase_'` "AXabcd"

    it "[intercalates]" $ do
      let s = eachForTest ['a' .. 'm']
          delim = yield '_' >> yield '#' >> yield '_'

      ( chunks 3 s
          & intercalates delim
        )
        `testStreamWithBase_'` "abc_#_def_#_ghi_#_jkl_#_m_#_"

  describe "zipping" $ do
    it "[separate]" $ do
      let s = do
            _ <- yields (InL (0 :> empty_))
            _ <- yields (InL (1 :> empty_))
            _ <- yields (InR (100 :> empty_))
            _ <- yields (InR (200 :> empty_))
            yields (InL (2 :> empty_))

      (l, r) <-
        separate s
          & toListEffect_
      l `shouldBe` [0, 1, 2 :: Int]
      r `shouldBe` [100, 200 :: Int]

    it "[separate,unseparate]" $ do
      let stream = do
            _ <- yields (InL ((0 :: Int) :> empty_))
            emptyEffect
            _ <- yields (InL (1 :> empty_))
            _ <- yields (InR ((100 :: Int) :> empty_))
            emptyEffect
            _ <- yields (InR ((200 :: Int) :> empty_))
            _ <- yields (InL (1 :> empty_))
            _ <- yields (InR (300 :> empty_))
            yields (InL (2 :> empty_))
      ( separate stream
          & unseparate
          & maps
            ( \case
                InL (x :> s) -> ("left:" ++ show x :> s)
                InR (x :> s) -> ("right:" ++ show x :> s)
            )
          & (>> empty_)
        )
        `testStreamWithBase_'` [ "left:0",
                                 "left:1",
                                 "right:100",
                                 "right:200",
                                 "left:1",
                                 "right:300",
                                 "left:2"
                               ]

    it "[zipsWith]" $ do
      let s1 = eachForTest [1 .. 5 :: Int]
          s2 = eachForTest [100 .. 2000 :: Int]
      ( zipsWith (\(x :> s) (y :> s') -> x + y :> (s, s')) s1 s2
          & S.map (* 2)
        )
        `testStreamWithBase_'` [202, 206, 210, 214, 218]
      ( zipsWith (\(x :> s) (y :> s') -> x + y :> (s, s')) s1 s2
          & S.map (* 2)
        )
        `testStreamWithBase_'` [202, 206, 210, 214, 218]

    it "[decompose]" $ do
      let s = eachForTest [1 .. 5 :: Int]
      (captured, d :> _) <-
        captureOutput $
          S.map (* 2) s
            & maps (Compose . S._first printEffect)
            & decompose
            & toList
      captured `shouldBe` "2\n4\n6\n8\n10\n"
      d `shouldBe` [2, 4, 6, 8, 10]

    it "[expand,expandPost]" $ do
      let s = eachForTest [1 .. 5 :: Int]
      (l, r) <-
        expand (\inner (x :> xs) -> x * 100 :> inner (x * 10 :> xs)) s
          & toListEffect_
      l `shouldBe` [10, 20, 30, 40, 50]
      r `shouldBe` [100, 200, 300, 400, 500]

      (l', r') <-
        expandPost (\inner (x :> xs) -> x * 100 :> inner (x * 10 :> xs)) s
          & toListEffect_
      l `shouldBe` l'
      r `shouldBe` r'

    describe "[zips,unzipped]" $ do
      it "is invertible" $ do
        let zipped = zips (eachForTest ['a' .. 'd']) (eachForTest [1 .. 4])
            unzipped = unzips zipped
        (v2, v1) <- toListEffect_ unzipped
        v1 `shouldBe` [1, 2, 3, 4 :: Int]
        v2 `shouldBe` "abcd"

      it "truncates" $ do
        let s2 = each "abcd"
            zipped = zips (repeats (1 :> ())) s2
            unzipped = unzips zipped
        (v2, v1) <- toListEffect_ unzipped
        v1 `shouldBe` "abcd"
        v2 `shouldBe` [1, 1, 1, 1 :: Int]

    it "[interleave]" $ do
      let s1 = do
            yield "a"
            emptyEffect @IO
            yield "b"
            emptyEffect @IO
            yield "c"

          s2 = do
            emptyEffect @IO
            emptyEffect @IO
            yield "1"
            yield "2"
            yield "3"
            yield "4"
            yield "5"
            yield "6"

      interleaves s1 s2
        `testStreamWithBase_'` [ "a1",
                                 "b2",
                                 "c3" :: String
                               ]

preludeSpec :: SpecWith ()
preludeSpec = describe "Prelude" $ do
  it "[iterate]" $ do
    ( S.iterate (* 2) 1
        & takes 5
      )
      `testStreamWithBase_'` [1, 2, 4, 8, 16 :: Int]
    ( S.iterateM (pure . (* 2)) (pure 1)
        & takes 5
      )
      `testStreamWithBase_'` [1, 2, 4, 8, 16 :: Int]

  describe "Consuming" $ do
    it "[stdoutLn]" $ do
      (out, result) <-
        captureOutput $
          eachForTest @IO [1 .. 10 :: Int]
            & S.map show
            & S.stdoutLn
      result `shouldBe` ()
      out `shouldBe` "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"

  describe "Transforming" $ do
    it "[map]" $ do
      ( eachForTest [1 .. 4 :: Int]
          & S.map (* 2)
          & S.map show
        )
        `testStreamWithBase_'` ["2", "4", "6", "8"]

    it "[for]" $ do
      ( eachForTest [97, 100, 120]
          & flip S.for (\a -> S.replicate 3 (chr a) >> S.yield ' ')
        )
        `testStreamWithBase_'` "aaa ddd xxx "

    it "[with]" $ do
      ( eachForTest [1 .. 3 :: Int]
          & flip S.with (\a -> replicates a (a :> ()))
          & concats
        )
        `testStreamWithBase_'` [1, 2, 2, 3, 3, 3]

    describe "[copy]" $ do
      it "should copy" $ do
        (out, res) <-
          captureOutput $
            eachForTest "haskell"
              & S.copy
              & S.map ord
              & S.mapM (liftIO . putStr . show)
              & S.effects
              & S.map toUpper
              & S.mapM putChar
              & S.effects
        res `shouldBe` ()
        out `shouldBe` "104H97A115S107K101E108L108L"

      it "should produce the same result regardless the streams are processed" $ do
        let str = eachForTest "haskell"
            mapping =
              S.map toUpper
                >>> S.mapM putChar

        -- case: process outer first
        (out, _) <-
          captureOutput $
            str
              & S.copy
              & S.map ord
              & S.mapM (liftIO . putStr . show)
              & S.effects
              & mapping
              & S.effects
        out `shouldBe` "104H97A115S107K101E108L108L"

        -- case: process inner first
        (out2, _) <-
          captureOutput $
            str
              & S.copy
              & hoist mapping
              & hoist S.effects
              & S.map ord
              & S.mapM (putStr . show)
              & S.effects
        out2 `shouldBe` out

    it "[chain]" $ do
      (captured, _) <-
        captureOutput $
          eachForTest [1 .. 10 :: Int]
            & S.chain (putStr . show)
            & S.effects
      captured `shouldBe` "12345678910"

    it "[sequence] interleaves the effects" $ do
      (captured, x :> _) <-
        captureOutput $
          eachForTest [1 .. 3 :: Int]
            & S.map (\x -> fmtLn ("value: " +|| x ||+ "") >> fmtLn ("even: " +|| even x ||+ "") >> pure x)
            & S.sequence
            & S.toList
      captured `shouldBe` "value: 1\neven: False\nvalue: 2\neven: True\nvalue: 3\neven: False\n"
      x `shouldBe` [1, 2, 3]

    it "[filter]" $ do
      ( eachForTest [1 .. 10 :: Int]
          & S.filter even
        )
        `testStreamWithBase_'` [2, 4, 6, 8, 10]

      ( eachForTest [1 .. 10 :: Int]
          & S.filterM (pure . odd)
        )
        `testStreamWithBase_'` [1, 3, 5, 7, 9]

    it "[concat]" $ do
      ( eachForTest [1 .. 4 :: Int]
          & S.map (\x -> [1 .. x])
          & S.concat
        )
        `testStreamWithBase_'` [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]

  describe "Folding" $ do
    it "[fold] folds with effects" $ do
      let s = do
            S.yield (1 :: Int)
            effect $ putStrLn "Step 1"
            S.yield 10
            effect $ putStrLn "Step 2"
            S.yield 100
            S.yield 200
            effect $ putStrLn "done"

      (out, items :> r) <-
        captureOutput $
          S.fold
            (flip (:))
            []
            id
            s
      out `shouldBe` "Step 1\nStep 2\ndone\n"
      items `shouldBe` [200, 100, 10, 1]
      r `shouldBe` ()

    it "[foldM] folds with effects" $ do
      let s = do
            S.yield (1 :: Int)
            effect $ putStrLn "Step 1"
            S.yield 10
            effect $ putStrLn "Step 2"
            S.yield 100
            S.yield 200
            effect $ putStrLn "done"

      (out, items :> r) <-
        captureOutput $
          S.foldM
            (\items v -> (: items) <$> printEffect v)
            (pure [])
            pure
            s
      out `shouldBe` "1\nStep 1\n10\nStep 2\n100\n200\ndone\n"
      items `shouldBe` [200, 100, 10, 1]
      r `shouldBe` ()

    it "[all,any]" $ do
      let allEven = eachForTest [0, 2 .. 10 :: Int]
          allOdd = eachForTest [1, 3 .. 9 :: Int]

      S.all even allEven >>= (`shouldBe` True) . S.fst'
      S.all even allOdd >>= (`shouldBe` False) . S.fst'
      S.all even (allEven >> allOdd) >>= (`shouldBe` False) . S.fst'
      S.any even allEven >>= (`shouldBe` True) . S.fst'
      S.any even allOdd >>= (`shouldBe` False) . S.fst'
      S.any even (allEven >> allOdd) >>= (`shouldBe` True) . S.fst'

    it "[sum, product, head, elem, notElem]" $ do
      l <- randomList @Int 10
      let num1 = eachForTest [1 .. 10 :: Int]
          randomStream = eachForTest l

      S.sum randomStream >>= (`shouldBe` sum l) . S.fst'
      S.sum emptyInt >>= (`shouldBe` (0 :: Int)) . S.fst'
      S.product randomStream >>= (`shouldBe` product l) . S.fst'
      S.product emptyInt >>= (`shouldBe` (1 :: Int)) . S.fst'

      S.head randomStream >>= (`shouldBe` Just (head l)) . S.fst'
      S.head emptyInt >>= (`shouldBe` Nothing) . S.fst'
      S.last randomStream >>= (`shouldBe` Just (last l)) . S.fst'
      S.last emptyInt >>= (`shouldBe` Nothing) . S.fst'

      S.elem 1 num1 >>= (`shouldBe` True) . S.fst'
      S.elem 100 num1 >>= (`shouldBe` False) . S.fst'
      S.notElem 1 num1 >>= (`shouldBe` False) . S.fst'
      S.notElem 100 num1 >>= (`shouldBe` True) . S.fst'

  describe "Splitting" $ do
    it "[next]" $ do
      let l = eachForTest [1 .. 10 :: Int]
      S.next emptyInt >>= (`shouldBe` Left ()) . (fst <$>)
      S.next l >>= (`shouldBe` Right 1) . (fst <$>)
    
    describe "break group" $ do
      let testWithCapture :: (Eq a, Show a) => StreamOf a IO () -> (String, [a]) -> IO ()
          testWithCapture str expected = do
            (captured, l) <- captureOutput $ S.toList_ str
            captured `shouldBe` fst expected
            l `shouldBe` snd expected
          emptyWithPrint :: String -> StreamOf a IO ()
          emptyWithPrint s = effect (putStr s >> pure ())

      it "[breakWhen]" $ do
        (l1 :> rest) <-
          eachForTest [1 .. 15 :: Int]
            & S.breakWhen (+) 0 id (> 30)
            & S.toList
        l2 <- S.toList_ rest
        l1 `shouldBe` [1 .. 7]
        l2 `shouldBe` [8 .. 15]

      it "[breakWhen] may never break into all + empty" $ do
        (l1 :> rest) <-
          eachForTest [1 .. 10 :: Int]
            & S.break (> 1000)
            & S.toList
        l2 <- S.toList_ rest
        l1 `shouldBe` [1 .. 10]
        l2 `shouldBe` []

      it "[breakWhen] may break break into empty + all" $ do
        (l :> rest2) <-
          eachForTest [1 .. 10 :: Int]
            & S.break (> -1000)
            & S.toList
        l2 <- S.toList_ rest2
        l `shouldBe` []
        l2 `shouldBe` [1 .. 10]

      it "[break]" $ do
        (l1 :> rest) <-
          eachForTest [1 .. 10 :: Int]
            & S.break (> 4)
            & S.toList
        l2 <- S.toList_ rest
        l1 `shouldBe` [1 .. 4]
        l2 `shouldBe` [5 .. 10]

      it "[breaks]" $ do
        (captured, l :> _) <-
          captureOutput $
            each "aaa   b   cc    dddd   "
              & S.breaks (== ' ')
              & mapped toListAndPrint
              & S.toList

        l `shouldBe` ["aaa", "b", "cc", "dddd"]
        captured `shouldBe` "[aaa][b][cc][dddd]"

      it "[breaks] may have empty breaks" $ do
        (l :> ()) <-
            each "     aaa   b   "
              & S.breaks (== ' ')
              & mapped toListAndPrint
              & S.toList
        l `shouldBe` ["aaa", "b"]

      it "[breaks] preserves effect" $ do
        (do
              S.yield 'a'
              emptyWithPrint "e1"
              S.yield ' '
              emptyWithPrint "e2"
              S.yield 'a'
            & S.breaks (== ' ')
            & mapped S.toList
          )`testWithCapture` ("e1e2", ["a", "a"])

      it "[breaks] preserves effect between empty breaks" $ do
        (do
              S.yield 'a'
              emptyWithPrint "e1"
              S.yield ' '
              emptyWithPrint "e2"
              S.yield ' '
              S.yield ' '
              S.yield ' '
              emptyWithPrint "e3"
              S.yield ' '
              emptyWithPrint "e4"
              S.yield 'a'
            & S.breaks (== ' ')
            & mapped S.toList
          )`testWithCapture` ("e1e2e3e4", ["a", "", "", "a"])

      it "[group]" $ do
        (l :> _) <-
          each "baaaaad"
            & S.group
            & mapped S.toList
            & S.toList
        l `shouldBe` ["b", "aaaaa", "d"]

      it "[groupBy]" $ do
        (l :> _) <-
          each [1, 2, 3, 3, 4, 3, 2, 1, 4, 5, 6, 7, 6, 5 :: Int]
            & S.groupBy (>=)
            & mapped S.toList
            & S.toList
        l
          `shouldBe` [ [1],
                       [2],
                       [3, 3],
                       [4, 3, 2, 1, 4],
                       [5],
                       [6],
                       [7, 6, 5]
                     ]

extraSpec :: SpecWith ()
extraSpec = describe "extraSpec" $ do
  it "[zipPair]" $ do
    let s1 = eachForTest [1 .. 10]
        s2 = eachForTest ["a", "b", "c", "d"]
    zipPair s1 s2
      `testStreamWithBase_'` [(1 :: Int, "a" :: String), (2, "b"), (3, "c"), (4, "d")]

  it "[logStreamInWriter]" $ do
    let s :: StreamOf Int (IOWriter [String]) ()
        s = eachForTest [1 .. 5]
        f = logStreamInWriter (\a -> [show $ 2 * a]) >>> toList_
    (_, logData) <- runWriterT $ f s
    logData `shouldBe` ["2", "4", "6", "8", "10"]
