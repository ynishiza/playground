{-# LANGUAGE TemplateHaskell #-} 

module Chapter13.Nat.TH (
  genNumAliases,
  ) where

import Language.Haskell.TH
import Data.Type.Nat

numNameS :: String
numNameS = "MyNum"

mkNmName :: Show a => a -> Name
mkNmName n = mkName $ numNameS ++ show n

genNumAliases :: Int -> Int -> Q [Dec]
genNumAliases l h = foldMap genNumAlias [l..h]

genNumAlias :: Int -> Q [Dec]
genNumAlias n = do
  let name = mkNmName n
      sig =
        if n == 0
          then [t|'Z|]
          else [t|'S $(conT $ mkNmName (n - 1))|]
  dec <- tySynD name [] sig
  return [dec]
