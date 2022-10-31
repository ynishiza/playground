import qualified TestBase
import qualified TestReadShow
import qualified TestModuleMtl
-- import qualified TestMonad
-- import qualified TestModuleTransformer
-- import qualified TestModuleMtl
-- import qualified TestMyStateMonad
-- import qualified TestStateMonadExample
-- import qualified TestTypeClass
import TestUtils

main :: IO ()
main =
  callTest
    ( do
        putStrLn "Run all? (y/n)"
        response <- getChar
        if response == 'y'
          then TestBase.runAll
          else do
            TestReadShow.testReadShow
            -- TestModuleMtl.testBinarySequenceState
            TestModuleMtl.testComposeState
            -- TestTypeClass.testDerivedInstance
            -- TestModuleMtl.testMyIOState
            -- TestMyStateMonad.testStateMonad
            -- TestStateMonadExample.runTest
            -- TestMonad.testMonadFix
            -- TestModuleTransformer.testLazyStateMonad
    )
    "main"
