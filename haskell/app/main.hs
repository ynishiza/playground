-- import Mtl.TestBase qualified
-- import TestReadShow qualified
-- import qualified TestMonad
-- import qualified TestModuleTransformer

-- import qualified TestModuleMtl
-- import qualified TestMyStateMonad
-- import qualified TestStateMonadExample
-- import qualified TestTypeClass

import Control.Monad
import Control.Monad.Trans.State
import TestBase qualified
import TestUtils
import Transformer.TestBase qualified

-- import Transformer.TestLabellingTrees qualified as TL

main :: IO ()
main = do
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
            -- TL.testTreeToNumber
            -- Transformer.TestBase.testStateWithAndWithoutMonads
            Transformer.TestBase.testReadWriteState
            -- TestTypeClass.testDerivedInstance
            -- TestModuleMtl.testMyIOState
            -- TestMyStateMonad.testStateMonad
            -- TestStateMonadExample.runTest
            -- TestMonad.testMonadFix
            -- TestModuleTransformer.testLazyStateMonad
        )
