-- import Mtl.TestBase qualified
-- import qualified TestMonad
-- import qualified TestModuleTransformer
-- import qualified TestModuleMtl
-- import qualified TestMyStateMonad
-- import qualified TestStateMonadExample
-- import qualified TestTypeClass
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE OverloadedStrings #-} 

-- import Control.Applicative
import Control.Monad
import Data.Foldable
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Cont
-- import Control.Monad.Trans.State
import Fmt
-- import Modules.TestOptparseApplicative qualified
import TestBase qualified
import TestMyOptParse qualified
import GHCExts.TestTypeFamilies
import qualified Modules.TestHspec
-- import TestMyOptParse2 qualified
-- import TestReadShow qualified
import Modules.TestRandom
import TestUtils
-- import Transformer.TestState qualified

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
-- main = Modules.TestHspec.main
main = main0
main0 = do
  scratchSpace
  putStrLn "Run all? (y/n)"
  response <- getChar
  if response == 'y'
    then runTest TestBase.allTests
    else
      runTest
        ( do
            -- GHCExts.TestTypeFamilies.testTypeFamily
            -- TestMyOptParse.runAll
            Modules.TestRandom.runAll
            -- TestMyOptParse2.runAll
            -- Modules.TestOptparseApplicative.test
            -- TestReadShow.unittestEquationReadShow
            -- TestReadShow.testBasicShow
            -- TestModuleMtl.testBinarySequenceState
            -- Mtl.TestBase.testComposeState
            -- LT.testTreeToNumber
            -- Transformer.TestBase.testStateWithAndWithoutMonads
            -- Transformer.TestState.testWriteState
            -- Transformer.TestState.testCont
            -- Transformer.TestState.testContWithIO
            -- Transformer.TestState.testDelimitedCont
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
  let (>>==) :: [a] -> (a -> [b]) -> [b]
      l1 >>== k
        | (x : xs) <- l1 = foldr (:) (xs >>== k) (k x)
        | otherwise = []
   in print $ [1 .. 10] >>== (\x -> [x, 10 * x]) >>== \x -> replicate 2 x

  let f x = do
        when (x `mod` 2 == 0) Nothing
        when (x `mod` 3 == 0) Nothing
        when (x `mod` 5 == 0) Nothing
        return x
   in do
      traverse_ (\x -> f x||+" ") [1..30]

  putStrLn "Scratch"
