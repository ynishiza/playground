{-# LANGUAGE OverloadedStrings #-}

module SimpleStreamTest
  ( test,
  )
where

import Control.Applicative hiding (empty)
import Control.Applicative qualified as M
import Control.Arrow ((>>>))
import Data.Bifunctor
import Fmt
import SimpleStream
import Text.Read
import Utils

test :: IO ()
test = do
  let str1 = do
        yield @Int 1
        yield 2
        yield 3
        yield 4

  printBannerWrap "stream: 1 2 3 4" $ do
    runStream str1
    fmt "sum\t:" >> (ssum str1 >>= print)
    fmt "collect:\t" >> (collects @[] str1 >>= print)
  enterNext

  printBannerWrap "stream3" $ do
    runStream $ do
          yield 100
          Effect $ putStr "read str1:" >> pure str1
          yield 300
  enterNext

  printBannerWrap "stdio stream" $ do
    let s = do
          promptOne @Int
          promptOne @Int
          promptOne @Int
        f :: Stream (Of Int) IO r -> Stream (Of Int) IO r
        f =
          do
            strShowItem
            >>> filtersOf even
            >>> mapOf (* 2)
            >>> strShowItem
            >>> mapOf (* 10)
            >>> strShowItem
    printStream $ f s
  enterNext


  printBannerWrap "read and apply" $ do
    n <- readNumbers @Int
    let s = each n
        f =
          filtersOf even
            >>> mapOf (* 10)
    runStream $ f s
  enterNext

  printBannerWrap "withEffect" $ do
    runStream $ withEffect (\x -> fmtLn $ "[EFFECT] element:" +||x||+"") str1

  printBannerWrap "zipPair" $ do
    let s1 = each [1 .. 10 :: Int]
        s2 = each [20 .. 24 :: Int]
    runStream $ zipPair s1 s2
  pure ()

enterNext :: IO ()
enterNext = do
  _ <- getLine
  pure ()

runStream :: (Show r, Show e) => Stream (Of e) IO r -> IO ()
runStream s = fmt "collect:\t" >> (collects @[] s >>= print)

readNumbers :: forall e. (Num e, Read e) => IO [e]
readNumbers = do
  fmt "Enter number:"
  r <- readMaybe @e <$> getLine
  maybe (pure []) (\n -> (n :) <$> readNumbers) r

collects :: forall t e m r. (Monad m, Alternative t) => Stream (Of e) m r -> m (Of (t e) r)
collects (Return r) = pure (M.empty :> r)
collects (Step (e :> s)) = first (pure e <|>) <$> collects s
collects (Effect e) = e >>= collects

strShowItem :: (Show e) => Stream (Of e) IO r -> Stream (Of e) IO r
strShowItem = withEffect (\a -> fmtLn $ "item:" +||a||+"")
-- strShowItem v@(Return _) = v
-- strShowItem (Step (e :> s)) = Effect $ do
--   liftIO $ fmtLn $ "item:" +|| e ||+ ""
--   return $ Step (e :> strShowItem s)
-- strShowItem (Effect e) = Effect $ strShowItem <$> e

filtersOf :: forall a m r. (a -> Bool) -> StreamOf a m r -> StreamOf a m r
filtersOf f = transform
  where
    transform :: StreamOf a m r -> StreamOf a m r
    transform (Return r) = Return r
    transform (Effect e) = Effect $ transform <$> e
    transform (Step (a :> s)) = if f a then Step (a :> transform s) else transform s
