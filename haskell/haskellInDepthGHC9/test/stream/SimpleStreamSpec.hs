{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module SimpleStreamSpec
  ( spec,
    ofToTuple,
    tupleToOf,
  )
where

import Control.Applicative hiding (empty)
import Control.Applicative as M
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.List (isInfixOf)
import Data.Time.Clock.POSIX
import SimpleStream.Stream
import Test.Hspec

oneSecond :: Int
oneSecond = 1 * 1000 * 1000

ofToTuple :: Of a b -> (a, b)
ofToTuple (a :> b) = (a, b)

tupleToOf :: (a, b) -> Of a b
tupleToOf (a, b) = a :> b

collectsOf :: forall t e m r. (Monad m, Alternative t) => StreamOf e m r -> m (Of (t e) r)
collectsOf = (tupleToOf <$>) . collects (\(a :> str) -> return (pure a, str))

collects :: forall t a f m r. (Monad m, Alternative t) => (forall x. f x -> m (t a, x)) -> Stream f m r -> m (t a, r)
collects f = loop
  where
    loop :: Stream f m r -> m (t a, r)
    loop (Return r) = return (M.empty, r)
    loop (Effect e) = e >>= loop
    loop (Step s) = appendStep $ f $ loop <$> s

    appendStep :: m (t a, m (t a, r)) -> m (t a, r)
    appendStep x = do
      (a, res) <- x
      first (a <|>) <$> res

testStream :: (Show a, Eq a, Show r, Eq r) => ([a], r) -> StreamOf a IO r -> IO ()
testStream (expected, r) str = collectsOf str >>= (`shouldBe` (expected :> r))

getTimestamp :: IO Double
getTimestamp = fromRational . toRational <$> getPOSIXTime

allInRange :: Ord e => e -> e -> [e] -> Bool
allInRange x y = all (inRange x y)

inRange :: Ord e => e -> e -> e -> Bool
inRange x y v = x <= v && v <= y

testStream_ :: (Show a, Eq a) => [a] -> StreamOf a IO () -> IO ()
testStream_ expected = testStream (expected, ())

spec :: SpecWith ()
spec = describe "Simple Stream" $ do
  describe "Stream" $ do
    it "creates a stream with steps" $ do
      let s = do
            yield 1
            yield 2
      testStream_ @Int [1, 2] s

    it "creates a stream with effects" $ do
      startTime <- getTimestamp
      let s = do
            yield 1
            Effect $ do
              threadDelay oneSecond
              each <$> replicateM 2 getTimestamp
            Effect $ do
              each <$> replicateM 3 getTimestamp
            yield 4
      (res :> _) <- collectsOf @[] s
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
      testStream_ @Int [3, 1, 2, 10] s2

  describe "maps" $ do
    it "maps steps" $ do
      testStream_ @String ["10", "20", "30", "40"] $
        maps (\(a :> str) -> show a :> str) $
          maps (\(a :> str) -> 10 * a :> str) $
            each [1 .. 4 :: Int]

    describe "mapOf" $ do
      it "maps elements" $ do
        testStream_ ["2", "4", "6", "8"] $
          mapOf show $
            mapOf (* 2) $
              each [1 .. 4 :: Int]

  describe "zipsWith" $ do
    describe "zipPair" $ do
      it "zips two streams" $ do
        let s1 = each [1 .. 10]
            s2 = each ["a", "b", "c", "d"]
        testStream_ @(Int, String) [(1, "a"), (2, "b"), (3, "c"), (4, "d")] $ zipPair s1 s2

  describe "withEffect" $ do
    it "applies effect" $ do
      startTime <- getTimestamp
      (res :> _) <-
        collectsOf @[] $
          withEffectMap (\a -> (a,) <$> getTimestamp) $
            each [1 .. 4 :: Int]
      endTime <- getTimestamp

      fst <$> res `shouldBe` [1, 2, 3, 4]
      snd <$> res `shouldSatisfy` all (inRange startTime endTime)
      length res `shouldBe` 4

  describe "chunking" $ do
    describe "splitsAt" $ do
      let extractSplit :: Monad m => StreamOf a m (StreamOf a m r) -> m ([a], [a])
          extractSplit str = do
            (x :> s) <- collectsOf @[] str
            (y :> _) <- collectsOf @[] s
            return (x, y)

      it "splits the stream" $ do
        let s = each [1 .. 5 :: Int]
        (x, y) <- extractSplit $ splitsAt 2 s
        x `shouldBe` [1, 2]
        y `shouldBe` [3, 4, 5]

      it "preserves effect" $ do
        startTime <- getTimestamp
        let s =
              withEffectMap (\a -> (a,) <$> getTimestamp) $
                mapOf (* 10) $
                  each [1 .. 5 :: Int]
        (x, y) <- extractSplit $ splitsAt 2 s
        endTime <- getTimestamp
        fst <$> x `shouldBe` [10, 20]
        snd <$> x `shouldSatisfy` allInRange startTime endTime
        fst <$> y `shouldBe` [30, 40, 50]

      it "splits the stream at 0" $ do
        let s = each [1 .. 5 :: Int]
        (x, y) <- extractSplit $ splitsAt 0 s
        x `shouldBe` []
        y `shouldBe` [1 .. 5]

      it "splits the stream longer" $ do
        let s =
              each [1 .. 5 :: Int]
        (x, y) <- extractSplit $ splitsAt 100 s
        x `shouldBe` [1 .. 5]
        y `shouldBe` []

  describe "chunks" $ do
    let extractChunk2 :: forall a m r. Monad m => Stream (StreamOf a m) m r -> m ([[a]], r)
        extractChunk2 =
          collects
            ( \s -> do
                (x :> r) <- collectsOf s
                return ([x], r)
            )

    it "splits the stream into chunks" $ do
      let s = each [1 .. 10 :: Int]
      (r, _) <- extractChunk2 $ chunks 3 s
      r
        `shouldBe` [ [1, 2, 3],
                     [4, 5, 6],
                     [7, 8, 9],
                     [10]
                   ]

    it "throws an error if the chunk size is not positive" $ do
      let s = each @Int @IO [1 .. 4 :: Int]
      evaluate (chunks 0 s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)
      evaluate (chunks (-10) s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)
