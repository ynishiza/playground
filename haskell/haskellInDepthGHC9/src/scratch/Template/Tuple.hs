module Template.Tuple (ithOfTuple) where

import Control.Monad
import Language.Haskell.TH

ithOfTuple :: Int -> Int -> Q Exp
ithOfTuple n i = do
  when (n <= i) $ fail $ "Error: " <> show n <> " <= " <> show i
  name <- newName "x"
  let pickNth :: [Q Pat]
      pickNth = (\j -> if j == i then varP name else wildP) <$> [0 .. (n - 1)]
  lamE
    [tupP pickNth]
    (varE name)
