{-# HLINT ignore "Use map once" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Alternative law, right identity" #-}

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
import Data.Char
import Data.Function
import Data.Functor.Sum
import Data.IORef
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Fmt
import SimpleStream.Prelude as S
import SimpleStream.Stream
import Test.Hspec

oneSecond :: Int
oneSecond = 1 * 1000 * 1000

withTrivialEffect :: (Monad m, Functor f) => Stream f m r -> Stream f m r
withTrivialEffect = mapsM pure

eachWithTrivial :: Monad m => [a] -> StreamOf a m ()
eachWithTrivial v = withTrivialEffect $ each v

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

spec :: SpecWith ()
spec = describe "Simple Stream" $ do
  describe "Stream" $ do
    describe "Basics" $ do
      it "creates a stream with steps" $ do
        let s = do
              yield 1
              yield 2
        testStreamWithBase_ @Int [1, 2] s

      it "creates a stream with effects" $ do
        startTime <- getTimestamp
        let s = do
              yield 1
              Effect $ do
                threadDelay oneSecond
                eachWithTrivial <$> replicateM 2 getTimestamp
              Effect $ do
                eachWithTrivial <$> replicateM 3 getTimestamp
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
        testStreamWithBase_ @Int [3, 1, 2, 10] s2

      it "[Monoid] sum" $ do
        let s1 = eachWithTrivial ["a", "b", "c"]
            s2 = eachWithTrivial ["w", "x", "y", "z"]
        testStreamWithBase_ @String ["aw", "bx", "cy"] $ do
          s1 <|> s2
        testStreamWithBase_ @String ["wa", "xb", "yc"] $ do
          s2 <|> s1

      -- Laws
      it "[Monoid] empty" $ do
        let s1 = eachWithTrivial ["a", "b", "c"]
        testStreamWithBase_ @String ["a", "b", "c"] $ do
          empty <|> s1
        testStreamWithBase_ @String ["a", "b", "c"] $ do
          s1 <|> empty

    describe "Constructing streams" $ do
      it "[replicates]" $ do
        testStreamWithBase_ @Int [1, 1, 1, 1] $
          replicates 4 (1 :> ())

      it "[repeats]" $ do
        let s = repeats (1 :> ())
        testStreamWithBase_ @Int [1, 1] $
          takes 2 s
        testStreamWithBase_ @Int [1, 1, 1, 1] $
          takes 4 s

      it "[repeatsM]" $ do
        let s = repeatsM $ return (1 :> ())
        testStreamWithBase_ @Int [1, 1] $
          takes 2 s
        testStreamWithBase_ @Int [1, 1, 1, 1] $
          takes 4 s

      it "[unfolds]" $ do
        testStreamWithBase_ @Int [0, 10, 20, 30, 40] $
          unfold (\x -> return $ if x < 5 then Right (x * 10 :> x + 1) else Left ()) 0

    describe "Transforming streams" $ do
      it "[maps]" $ do
        testStreamWithBase_ @String ["10", "20", "30", "40"] $
          maps (\(a :> str) -> show a :> str) $
            maps (\(a :> str) -> 10 * a :> str) $
              eachWithTrivial [1 .. 4 :: Int]

      it "[map]" $ do
        testStreamWithBase_ ["2", "4", "6", "8"] $
          S.map show $
            S.map (* 2) $
              eachWithTrivial [1 .. 4 :: Int]
      it "[withEffect]" $ do
        startTime <- getTimestamp
        (res :> _) <-
          toList $
            withEffectMap (\a -> (a,) <$> getTimestamp) $
              eachWithTrivial [1 .. 4 :: Int]
        endTime <- getTimestamp

        fst <$> res `shouldBe` [1, 2, 3, 4]
        snd <$> res `shouldSatisfy` all (inRange startTime endTime)
        length res `shouldBe` 4

      it "[group]" $ do
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
                >>> mapsM toList

        testStreamWithBase_
          [ ["L:3"],
            ["L:5"],
            ["R:-100"],
            ["R:-200"],
            ["L:7"],
            ["R:-100"],
            ["L:5"]
          ]
          $ f s

    describe "Zipping streams" $ do
      it "[zipPair]" $ do
        let s1 = eachWithTrivial [1 .. 10]
            s2 = eachWithTrivial ["a", "b", "c", "d"]
        testStreamWithBase_ @(Int, String) [(1, "a"), (2, "b"), (3, "c"), (4, "d")] $ zipPair s1 s2

    describe "Splitting and joining streams" $ do
      describe "[splitsAt]" $ do
        let extractSplit :: Monad m => StreamOf a m (StreamOf a m r) -> m ([a], [a])
            extractSplit str = do
              (x :> s) <- toList str
              (y :> _) <- toList s
              return (x, y)

        it "splits the stream" $ do
          let s = eachWithTrivial [1 .. 5 :: Int]
          (x, y) <- extractSplit $ splitsAt 2 s
          x `shouldBe` [1, 2]
          y `shouldBe` [3, 4, 5]

        it "preserves effect" $ do
          startTime <- getTimestamp
          let s =
                withEffectMap (\a -> (a,) <$> getTimestamp) $
                  S.map (* 10) $
                    eachWithTrivial [1 .. 5 :: Int]
          (x, y) <- extractSplit $ splitsAt 2 s
          endTime <- getTimestamp
          fst <$> x `shouldBe` [10, 20]
          snd <$> x `shouldSatisfy` allInRange startTime endTime
          fst <$> y `shouldBe` [30, 40, 50]

        it "splits the stream at 0" $ do
          let s = eachWithTrivial [1 .. 5 :: Int]
          (x, y) <- extractSplit $ splitsAt 0 s
          x `shouldBe` []
          y `shouldBe` [1 .. 5]

        it "splits the stream longer" $ do
          let s =
                eachWithTrivial [1 .. 5 :: Int]
          (x, y) <- extractSplit $ splitsAt 100 s
          x `shouldBe` [1 .. 5]
          y `shouldBe` []

    describe "[chunks]" $ do
      let extractChunk :: forall a m r. Monad m => ChunkedStream (Of a) m r -> m ([[a]], r)
          extractChunk =
            collects
              ( \s -> do
                  (x :> r) <- toList s
                  return ([x], r)
              )

      it "splits the stream into chunks" $ do
        let s = eachWithTrivial [1 .. 10 :: Int]
        (r, _) <- extractChunk $ chunks 3 s
        r
          `shouldBe` [ [1, 2, 3],
                       [4, 5, 6],
                       [7, 8, 9],
                       [10]
                     ]

      it "throws an error if the chunk size is not positive" $ do
        let s = eachWithTrivial @IO [1 .. 4 :: Int]
        evaluate (chunks 0 s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)
        evaluate (chunks (-10) s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)

      it "[concats]" $ do
        let s = chunks 3 $ eachWithTrivial [1 .. 10 :: Int]
        extractChunk s >>= (`shouldBe` 4) . length . fst
        testStreamWithBase_ [1 .. 10] $ concats s

    describe "[copy]" $ do
      let logCharRaw :: (MonadIO m) => (forall n. MonadIO n => Char -> n ()) -> Stream (Of Char) m r -> m r
          logCharRaw = logCharWith

          logCharCode :: (MonadIO m) => (forall n. MonadIO n => Int -> n ()) -> Stream (Of Char) m r -> m r
          logCharCode l = logCharWith (l . ord)

          logCharRawAndCodeWithRef :: forall m r. (MonadIO m) => IORef T.Text -> Stream (Of Char) m r -> m r
          logCharRawAndCodeWithRef ref = logCharRawAndCodeWith writeLog
            where
              writeLog :: MonadIO n => T.Text -> n ()
              writeLog txt = liftIO $ modifyIORef ref (`T.append` ("" +| txt |+ " "))
          logCharWith :: (Monad m) => (Char -> m ()) -> Stream (Of Char) m r -> m r
          logCharWith = S.mapM_

          logCharRawAndCodeWith :: forall m r. (MonadIO m) => (forall n. MonadIO n => T.Text -> n ()) -> Stream (Of Char) m r -> m r
          logCharRawAndCodeWith f str = logCharCode (writeLog . pretty) x
            where
              writeLog :: forall n. MonadIO n => T.Text -> n ()
              writeLog txt = liftIO $ pretty txt >> f txt
              x = store (logCharRaw (writeLog . pretty)) str

      it "copies" $ do
        ref <- newIORef @T.Text ""
        logCharRawAndCodeWithRef ref $ each ("haskell" :: String)
        readIORef ref >>= (`shouldBe` "h 104 a 97 s 115 k 107 e 101 l 108 l 108 ")

        writeIORef ref ""
        logCharRawAndCodeWithRef ref $ eachWithTrivial ("haskell" :: String)
        readIORef ref >>= (`shouldBe` "h 104 a 97 s 115 k 107 e 101 l 108 l 108 ")

    describe "[zip,unzip]" $ do
      it "is invertible" $ do
        let 
            zipped = zips (eachWithTrivial ['a' .. 'd']) (eachWithTrivial [1 .. 4])
            unzipped = unzips zipped
        (v1 :> v2) <- toList $ toList_ unzipped
        v1 `shouldBe` [1, 2, 3, 4 :: Int]
        v2 `shouldBe` ['a' .. 'd']
        
      it "truncates" $ do
        let 
            s2 = each ['a' .. 'd']
            zipped = zips (repeats (1 :> ())) s2
            unzipped = unzips zipped
        (v1 :> v2) <- toList $ toList_ unzipped
        v1 `shouldBe` ['a' .. 'd']
        v2 `shouldBe` [1, 1, 1, 1 :: Int]
