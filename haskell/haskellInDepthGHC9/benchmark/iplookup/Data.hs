module Data (iPRangeDBFiles, baseDir) where

import Control.Arrow
import qualified Data.Text as T
import System.FilePath

baseDir :: FilePath
baseDir = "./benchmark/data/iplookup"

iPRangeDBFiles :: [(T.Text, FilePath)]
iPRangeDBFiles =
  second (baseDir </>)
    <$> [ ("small", "1.iprs"),
          ("medium", "2.iprs"),
          ("large", "3.iprs")
        ]
