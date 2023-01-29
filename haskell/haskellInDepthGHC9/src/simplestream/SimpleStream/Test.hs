{-# LANGUAGE OverloadedStrings #-}

module SimpleStream.Test
  ( test,
  )
where

import Control.Arrow ((>>>))
import Fmt
import SimpleStream.Prelude as S
import SimpleStream.Stream
import SimpleStream.Extra
import Text.Read
import Utils

test :: IO ()
test = do
  let stream1 = do
        yield @Int 1
        yield 2
        yield 3
        yield 4

  printBannerWrap "test: yield" $ do
    runStream "stream" stream1
    fmt "sum\t:" >> (ssum stream1 >>= print)
    fmt "collect:\t" >> (collectsOf @[] stream1 >>= print)
  enterNext

  printBannerWrap "stream3" $ do
    runStream "stream" $ do
          yield 100
          Effect $ putStr "read stream1:" >> pure stream1
          yield 300
  enterNext

  printBannerWrap "test: stream from stdio" $ do
    let s = do
          promptOne @Int
          promptOne @Int
          promptOne @Int
        f :: Stream (Of Int) IO r -> Stream (Of Int) IO r
        f =
          do
            strShowItem
            >>> filtersOf even
            >>> S.map (* 2)
            >>> strShowItem
            >>> S.map (* 10)
            >>> strShowItem
    runStream "stdio" $ f s
  enterNext


  printBannerWrap "test: read and apply" $ do
    n <- readNumbers @Int
    let s = each n
        f =
          filtersOf even
            >>> S.map (* 10)
    runStream "read and apply" $ f s
  enterNext

  printBannerWrap "test: zipPair" $ do
    let s1 = each [1 .. 10 :: Int]
        s2 = each [20 .. 24 :: Int]
    runStream "s1" s1 
    runStream "s2" s2
    runStream "zipPairt s1 s2" $ zipPair s1 s2

  printBannerWrap "test: withEffect" $ do
    runStream "withEffect" $ withEffect (\x -> fmtLn $ "[EFFECT] element:" +||x||+"") stream1
  pure ()

enterNext :: IO ()
enterNext = do
  _ <- getLine
  pure ()

runStream :: (Show r, Show e) => String -> Stream (Of e) IO r -> IO ()
runStream name s = fmt (name ||+":\t") >> (collectsOf @[] s >>= print)

readNumbers :: forall e. (Num e, Read e) => IO [e]
readNumbers = do
  fmt "Enter number:"
  r <- readMaybe @e <$> getLine
  maybe (pure []) (\n -> (n :) <$> readNumbers) r

-- collects :: forall t e m r. (Monad m, Alternative t) => Stream (Of e) m r -> m (Of (t e) r)
-- collects (Return r) = pure (M.empty :> r)
-- collects (Step (e :> s)) = first (pure e <|>) <$> collects s
-- collects (Effect e) = e >>= collects


strShowItem :: (Show e) => Stream (Of e) IO r -> Stream (Of e) IO r
strShowItem = withEffect (\a -> fmtLn $ "item:" +||a||+"")
-- strShowItem v@(Return _) = v
-- strShowItem (Step (e :> s)) = Effect $ do
--   liftIO $ fmtLn $ "item:" +|| e ||+ ""
--   return $ Step (e :> strShowItem s)
-- strShowItem (Effect e) = Effect $ strShowItem <$> e

filtersOf :: forall a m r. Monad m => (a -> Bool) -> StreamOf a m r -> StreamOf a m r
filtersOf f = transform
  where
    transform :: StreamOf a m r -> StreamOf a m r
    transform (Return r) = Return r
    transform (Effect e) = Effect $ transform <$> e
    transform (Step (a :> s)) = if f a then Step (a :> transform s) else transform s
