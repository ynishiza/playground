-- import Mtl.TestBase qualified
import TestReadShow qualified
-- import qualified TestMonad
-- import qualified TestModuleTransformer
-- import qualified TestModuleMtl
-- import qualified TestMyStateMonad
-- import qualified TestStateMonadExample
-- import qualified TestTypeClass
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

import Control.Monad
import Control.Applicative
-- import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State
import TestBase qualified
import TestUtils
import Transformer.TestState qualified

-- import Transformer.TestLabellingTrees qualified as LT

-- pure x = MyStateT (\s -> pure (x,s))
-- (MyStateT sf) <*> (MyStateT sx) = MyStateT (\s -> let
--   mf = sf s
--   mx = sx s
--   c (f,_) (x,_) = (f x, s)
--                                                    in c <$> mf <*> mx)
-- in pure (f x, s))
-- in undefined)

main :: IO ()
main = do
  scratchSpace
  putStrLn "Run all? (y/n)"
  response <- getChar
  if response == 'y'
    then runTest TestBase.allTests
    else
      runTest
        ( do
            -- TestReadShow.unittestEquationReadShow
            -- TestReadShow.testBasicShow
            -- TestModuleMtl.testBinarySequenceState
            -- Mtl.TestBase.testComposeState
            -- LT.testTreeToNumber
            -- Transformer.TestBase.testStateWithAndWithoutMonads
            -- Transformer.TestState.testWriteState
            -- Transformer.TestState.testCont
            -- Transformer.TestState.testContWithIO
            Transformer.TestState.testDelimitedCont
            -- Transformer.TestState.testNestedState
            -- TestTypeClass.testDerivedInstance
            -- TestModuleMtl.testMyIOState
            -- TestMyStateMonad.testStateMonad
            -- TestStateMonadExample.runTest
            -- TestMonad.testMonadFix
            -- TestModuleTransformer.testLazyStateMonad
        )

scratchSpace :: IO ()
scratchSpace = do
  let
    (>>==) :: [a] -> (a -> [b]) -> [b]
    l1 >>== k 
      | (x:xs) <- l1 = foldr (:) (xs >>== k) (k x)
      | otherwise = []

  print $ [1..10] >>== (\x -> [x,10 * x]) >>== \x -> replicate 2 x

  let 
    f :: Int -> Cont r Int
    f n = do
      unless (even n) $ return ()
      return n

    g :: Int -> Cont r Int
    g n = callCC (\exit -> do
      unless (even n) $ exit (-1)
      return n
                 )

  putStrLn "Scratch"
