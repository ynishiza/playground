{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Exception
import Fmt
import RadarSafe

main :: IO ()
main = run $ mkSomeSafeRadar (MkSafeRadar @'North)

instance Buildable (SafeRadar d) where build = build . show

run :: SomeSafeRadar -> IO ()
run r =
  (runStep r >>= again)
    `catch` ( \(e :: IOError) -> print e >> again r)
  where
    again :: SomeSafeRadar -> IO ()
    again r' = do
      fmtLn "Run again? (y/n)"
      response <- getLine
      if response == "y" then run r' else putStrLn "bye"

runStep :: SomeSafeRadar -> IO SomeSafeRadar
runStep (_ :&: r@MkSafeRadar) = do
  fmtLn $
    nameF "current" (build r)
      <> "Enter turn next direction:\n"
      <> "1) North \t2) East \t3) South \t4) West"
  input <- read @Int <$> getLine
  case input of
    1 -> mkSomeSafeRadar <$> rotateUnit r SNorth
    2 -> mkSomeSafeRadar <$> rotateUnit r SEast
    3 -> mkSomeSafeRadar <$> rotateUnit r SSouth
    4 -> mkSomeSafeRadar <$> rotateUnit r SWest
    _ -> throw $ userError $ fmtLn "Bad choice " +| input |+ ""
