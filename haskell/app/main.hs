-- import Mtl.TestBase qualified
-- import TestReadShow qualified
-- import qualified TestMonad
-- import qualified TestModuleTransformer
-- import qualified TestModuleMtl
-- import qualified TestMyStateMonad
-- import qualified TestStateMonadExample
-- import qualified TestTypeClass
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

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
            -- TestReadShow.testReadShow
            -- TestModuleMtl.testBinarySequenceState
            -- Mtl.TestBase.testComposeState
            -- LT.testTreeToNumber
            -- Transformer.TestBase.testStateWithAndWithoutMonads
            Transformer.TestState.testWriteState
            Transformer.TestState.testCont
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
  let task :: Show a => StateT a Maybe String
      task = do
        show <$> get
      task2 :: Num a => StateT a Maybe ()
      task2 = StateT (\s -> Just ((), s * 2))
      task3 :: Num a => StateT a Maybe ()
      task3 = do modify (* 2)

      updateState t = do
        v <- t
        put v
      multNGet :: Num a => a -> StateT a Maybe a
      multNGet n = do
        v <- get
        return (n * v)
      multNPut :: Num a => a -> StateT a Maybe ()
      multNPut n = do
        modify (* n)
      getApply :: (a -> b) -> StateT a Maybe b
      getApply f = do f <$> get
      printState :: Show a => StateT a Maybe String
      printState = do
        v <- get
        return (show v)

      raise2 :: (Traversable m) => m a -> StateT s m a
      -- raise2 m = StateT (traverse (,) m)
      raise2 = StateT . traverse (,)
      raise :: (Functor m) => m a -> StateT s m a
      -- raise mf = StateT (\s -> (,s)<$>mf)
      -- raise mf = StateT (\s -> (,s)<$>mf
      raise mf = StateT ((<$> mf) . flip (,))
      -- raise mf = StateT ((<$>mf).(flip (,)))
      -- raise mf = mapStateT (\mx -> (\x y -> (y, snd x)) <$> mx <*> mf) get

      taskA :: Num a => StateT a Maybe a
      taskA = gets (* 2)
      taskB :: Num a => StateT a Maybe ()
      taskB = modify (* 3)
      taskD :: (Show a, Num a) => StateT a Maybe String
      taskD = do
        v <- taskA
        put (v + 1)
        taskB
        taskB
        gets show
      taskE :: (Show a, Num a) => StateT a Maybe String
      taskE = do
        v1 <- taskD
        v2 <- taskD
        taskB
        return (v1 ++ "," ++ v2)

      branchTask :: Int -> Int -> StateT Int [] Int
      branchTask n m =
        let mf :: [Int -> Int]
            mf = (*) <$> [n .. m]
            ops :: StateT Int [] (Int -> Int)
            -- ops = foldr (\x accum -> pure x <|> accum) (pure id) [(*1)]
            ops = asum $ pure <$> mf
         in ops <*> get

  print $ runStateT task 0
  print $ runStateT task True
  print $ runStateT taskE 2
  print $ runStateT (branchTask 1 4) 2
  -- print $ runStateT (do v <- branchTask 1 4; put v; branchTask 1 4) 2
  print $ runStateT (do branchTask 1 4 >>= put; branchTask 1 4) 2
  -- print $ runStateT (do branchTask; branchTask) 2
  print $
    runStateT
      ( do
          (* 2) <$> get >>= put
          v <- multNGet 10
          put v
          v <- gets (* 2)
          put v
          multNPut 2
          multNPut 3
          gets show
      )
      1

  print $ runStateT task2 1.5

  let multn :: Int -> Int -> Cont r Int
      multn n x =
        return (n * x)
      cappend v s = return (v++s)

      doCont x = do
        v1 <- multn 2 x
        v2 <- multn 3 v1
        v3 <- multn 4 v2
        c1 <- cappend "a" (show v3)
        c2 <- cappend "b" c1
        cappend "c" c2
      rc = runCont (doCont 1) print
   in rc

  putStrLn "Scratch"
