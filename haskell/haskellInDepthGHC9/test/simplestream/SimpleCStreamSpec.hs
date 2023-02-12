{-# LANGUAGE TupleSections #-}
module SimpleCStreamSpec
  ( spec,
    preludeSpec,
  )
where

import Control.Monad.IO.Class
import Data.Function
import SimpleStream.CPrelude (CStreamOf)
import SimpleStream.CPrelude qualified as C
import SimpleStream.CStream
import SimpleStream.Of
import Test.Hspec

test :: (Show a, Eq a, Show r, Eq r) => CStreamOf a IO r -> Of [a] r -> IO ()
test c expected = C.toList c >>= liftIO . (`shouldBe` expected)

test' :: (Show a, Eq a) => CStreamOf a IO () -> [a] -> IO ()
test' c expected = test c (expected :> ())

spec :: Spec
spec = describe "CStream" $ do
  it "[take]" $ do
    let s = C.each [1 .. 10 :: Int]

    takec 0 s `test'` []
    takec 1 s `test'` [1]
    takec 2 s `test'` [1, 2]
    takec 5 s `test'` [1 .. 5]
    takec 100 s `test'` [1 .. 10]

  it "[splitAt]" $ do
    let toListAll :: Monad m => CStreamOf a m (CStreamOf a m r) -> m ([a],[a])
        toListAll c = do
          (l :> rest) <- C.toList c
          (l,) <$> C.toList_ rest

        s = C.each [1..10 :: Int]

    (splitAtc 0 s
      & toListAll) >>= (`shouldBe` ([], [1..10]))
    (splitAtc 1 s
      & toListAll) >>= (`shouldBe` ([1], [2..10]))
    (splitAtc 2 s
      & toListAll) >>= (`shouldBe` ([1, 2], [3..10]))
    (splitAtc 10 s
      & toListAll) >>= (`shouldBe` ([1..10], []))
    (splitAtc 20 s
      & toListAll) >>= (`shouldBe` ([1..10], []))

preludeSpec :: Spec
preludeSpec = describe "Prelude" $ do
  it "[each]" $ do
    C.each [1 .. 10] `test'` [1 .. 10 :: Int]
