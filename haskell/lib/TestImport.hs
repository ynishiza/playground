module TestImport
  ( x,
  )
where

import TestExport qualified hiding (MyADT (ADTA))

-- import qualified Data.Maybe (Maybe(Just))
x = TestExport.ADTB 1
