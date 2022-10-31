module TestImport (
  x,
) where

import qualified TestExport hiding (MyADT(ADTA)) 
-- import qualified Data.Maybe (Maybe(Just))
x = TestExport.ADTB 1
