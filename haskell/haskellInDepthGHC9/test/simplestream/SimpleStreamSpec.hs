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
import Control.Monad.Trans.Writer
import Data.Char
import Data.Function
import Data.Functor.Compose
import Data.Functor.Sum
import Data.List (isInfixOf)
import Data.Time.Clock.POSIX
import SimpleStream.Extra
import SimpleStream.Prelude as S
import SimpleStream.Stream
import Test.Hspec

emptyEffect :: (Monad m, Functor f) => Stream f m ()
emptyEffect = Effect $ return $ pure ()

oneSecond :: Int
oneSecond = 1 * 1000 * 1000

withTrivialEffect :: (Monad m, Functor f) => Stream f m r -> Stream f m r
withTrivialEffect = mapped pure

createTestStreamFromList :: Monad m => [a] -> StreamOf a m ()
createTestStreamFromList v = withTrivialEffect $ each v

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

type IOWriter a = WriterT a IO

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
      testStreamWithBase_ @Int [1, 2] s

    it "creates a stream with effects" $ do
      startTime <- getTimestamp
      let s = do
            yield 1
            Effect $ do
              threadDelay oneSecond
              createTestStreamFromList <$> replicateM 2 getTimestamp
            Effect $ do
              createTestStreamFromList <$> replicateM 3 getTimestamp
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
      let s1 = createTestStreamFromList ["a", "b", "c"]
          s2 = createTestStreamFromList ["w", "x", "y", "z"]
      testStreamWithBase_ @String ["aw", "bx", "cy"] $ do
        s1 <|> s2
      testStreamWithBase_ @String ["wa", "xb", "yc"] $ do
        s2 <|> s1

    -- Laws
    it "[Monoid] empty" $ do
      let s1 = createTestStreamFromList ["a", "b", "c"]
      testStreamWithBase_ @String ["a", "b", "c"] $ do
        empty <|> s1
      testStreamWithBase_ @String ["a", "b", "c"] $ do
        s1 <|> empty

  describe "Constructing streams" $ do
    it "[replicates]" $ do
      testStreamWithBase_ "aaaa" $
        replicates 4 ('a' :> ())

    it "[repeats]" $ do
      let s = repeats ('a' :> ())
      testStreamWithBase_ "aa" $
        takes 2 s
      testStreamWithBase_ "aaaa" $
        takes 4 s

    it "[repeatsM]" $ do
      let s = repeatsM $ return ('a' :> ())
      testStreamWithBase_ "aa" $
        takes 2 s
      testStreamWithBase_ "aaaa" $
        takes 4 s

    it "[unfolds]" $ do
      testStreamWithBase_ @Int [0, 10, 20, 30, 40] $
        unfold (\x -> return $ if x < 5 then Right (x * 10 :> x + 1) else Left ()) 0

  describe "Transforming streams" $ do
    it "[maps]" $ do
      testStreamWithBase_ @String ["10", "20", "30", "40"] $
        createTestStreamFromList [1 .. 4 :: Int]
          & maps (\(a :> str) -> 10 * a :> str)
          & maps (\(a :> str) -> show a :> str)

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

  describe "Splitting and joining streams" $ do
    describe "[splitsAt]" $ do
      it "splits the stream" $ do
        let s = createTestStreamFromList [1 .. 5 :: Int]
        (x, y) <- toListReturn_ $ splitsAt 2 s
        x `shouldBe` [1, 2]
        y `shouldBe` [3, 4, 5]

      it "preserves effect" $ do
        startTime <- getTimestamp
        let s = createTestStreamFromList [1 .. 5 :: Int]
            work =
              S.map (* 10)
                >>> withEffectMap (\a -> (a,) <$> getTimestamp)

        (x, y) <- toListReturn_ $ splitsAt 2 (work s)
        endTime <- getTimestamp
        fst <$> x `shouldBe` [10, 20]
        snd <$> x `shouldSatisfy` allInRange startTime endTime
        fst <$> y `shouldBe` [30, 40, 50]

      it "splits the stream at 0" $ do
        let s = createTestStreamFromList [1 .. 5 :: Int]
        (x, y) <- toListReturn_ $ splitsAt 0 s
        x `shouldBe` []
        y `shouldBe` [1 .. 5]

      it "splits the stream longer" $ do
        let s =
              createTestStreamFromList [1 .. 5 :: Int]
        (x, y) <- toListReturn_ $ splitsAt 100 s
        x `shouldBe` [1 .. 5]
        y `shouldBe` []

    let extractChunk :: forall a m r. Monad m => ChunkedStream (Of a) m r -> m ([[a]], r)
        extractChunk =
          collects
            ( \s -> do
                (x :> r) <- toList s
                return ([x], r)
            )
    describe "[chunks]" $ do
      it "splits the stream into chunks" $ do
        let s = createTestStreamFromList [1 .. 10 :: Int]
        (r, _) <- extractChunk $ chunks 3 s
        r
          `shouldBe` [ [1, 2, 3],
                       [4, 5, 6],
                       [7, 8, 9],
                       [10]
                     ]

      it "throws an error if the chunk size is not positive" $ do
        let s = createTestStreamFromList @IO [1 .. 4 :: Int]
        evaluate (chunks 0 s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)
        evaluate (chunks (-10) s) `shouldThrow` (\(e :: IOException) -> "chunk size" `isInfixOf` show e)

    it "[joins]" $ do
      let s = createTestStreamFromList ['a' .. 'd']
          s2 = do
            yield 'A'
            yield 'X'
            return s
      testStreamWithBase_ "AXabcd" $ joins s2

    it "[concats]" $ do
      let s = chunks 3 $ createTestStreamFromList [1 .. 10 :: Int]
      extractChunk s >>= (`shouldBe` 4) . length . fst
      testStreamWithBase_ [1 .. 10] $ concats s

    it "intercalates" $ do
      let s = createTestStreamFromList ['a' .. 'm']
          t = yield '_' >> yield '#' >> yield '_'

      testStreamWithBase_ "abc_#_def_#_ghi_#_jkl_#_m_#_" $
        chunks 3 s
          & intercalates t

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
      testStreamWithBase_
        [ "left:0",
          "left:1",
          "right:100",
          "right:200",
          "left:1",
          "right:300",
          "left:2"
        ]
        $ separate stream
          & unseparate
          & maps
            ( \case
                InL (x :> s) -> ("left:" ++ show x :> s)
                InR (x :> s) -> ("right:" ++ show x :> s)
            )
          & (>> empty_)

    it "[zipsWith]" $ do
      let s1 = createTestStreamFromList [1 .. 5 :: Int]
          s2 = createTestStreamFromList [100 .. 2000 :: Int]
      testStreamWithBase_ [202, 206, 210, 214, 218] $
        zipsWith (\(x :> s) (y :> s') -> x + y :> (s, s')) s1 s2
          & S.map (* 2)
      testStreamWithBase_ [202, 206, 210, 214, 218] $
        zipsWith (\(x :> s) (y :> s') -> x + y :> (s, s')) s1 s2
          & S.map (* 2)

    it "[decompose]" $ do
      let s = createTestStreamFromList [1 .. 5 :: Int]
      testStreamWithBase_ [2, 4, 6, 8, 10] $
        maps (Compose . pure . mapOf (* 2)) s
          & decompose

    describe "[zips,unzipped]" $ do
      it "is invertible" $ do
        let zipped = zips (createTestStreamFromList ['a' .. 'd']) (createTestStreamFromList [1 .. 4])
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
      testStreamWithBase_
        [ "a1",
          "b2",
          "c3" :: String
        ]
        $ interleaves s1 s2

preludeSpec :: SpecWith ()
preludeSpec = describe "Prelude" $ do
  it "[iterate]" $ do
    testStreamWithBase_ [1, 2, 4, 8, 16 :: Int] $
      S.iterate (* 2) 1
        & takes 5
    testStreamWithBase_ [1, 2, 4, 8, 16 :: Int] $
      S.iterateM (pure . (* 2)) (pure 1)
        & takes 5

  it "[map]" $ do
    testStreamWithBase_ ["2", "4", "6", "8"] $
      createTestStreamFromList [1 .. 4 :: Int]
        & S.map (* 2)
        & S.map show

  it "[copy]" $ do
    let work =
          copy
            >>> S.map toUpper
            >>> logStreamInWriter (: [])
            >>> toList_
            >>> S.map (show . ord)
            >>> logStreamInWriter id
            >>> toList_
    runWriterT (work $ each "haskell") >>= (`shouldBe` "H104A97S115K107E101L108L108") . snd
    runWriterT (work $ createTestStreamFromList "haskell") >>= (`shouldBe` "H104A97S115K107E101L108L108") . snd

extraSpec :: SpecWith ()
extraSpec = describe "extraSpec" $ do
  it "[withEffect]" $ do
    startTime <- getTimestamp
    (res :> _) <-
      toList $
        withEffectMap (\a -> (a,) <$> getTimestamp) $
          createTestStreamFromList [1 .. 4 :: Int]
    endTime <- getTimestamp

    fst <$> res `shouldBe` [1, 2, 3, 4]
    snd <$> res `shouldSatisfy` all (inRange startTime endTime)
    length res `shouldBe` 4

  it "[zipPair]" $ do
    let s1 = createTestStreamFromList [1 .. 10]
        s2 = createTestStreamFromList ["a", "b", "c", "d"]
    testStreamWithBase_ @(Int, String) [(1, "a"), (2, "b"), (3, "c"), (4, "d")] $ zipPair s1 s2

  it "[logStreamInWriter]" $ do
    let s :: StreamOf Int (IOWriter [String]) ()
        s = createTestStreamFromList [1 .. 5]
        f = logStreamInWriter (\a -> [show $ 2 * a]) >>> toList_
    (_, lg) <- runWriterT $ f s
    lg `shouldBe` ["2", "4", "6", "8", "10"]
