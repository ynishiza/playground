module Lens.TH.Utils
  ( getVarNamesInType,
    replaceVarNamesInNames,
    replaceVarNamesInType,
    assignNewTypeVariables,
  )
where

import Data.Set as S hiding (foldr)
import Language.Haskell.TH
import Control.Monad

assignNewTypeVariables :: ([Name], Type) -> Q ([Name], Type)
assignNewTypeVariables (tyVarNames, fieldTy) = do
  newVarNames <- replicateM (size toReplace) $ newName "a"
  let oldNew = zip (S.toList toReplace) newVarNames
      newFieldTy = foldr (flip replaceVarNamesInType) fieldTy oldNew
      newTyVarNames = foldr (flip replaceVarNamesInNames) tyVarNames oldNew
  return (newTyVarNames, newFieldTy)
  where
    fieldVars = getVarNamesInType fieldTy
    toReplace = intersection fieldVars (S.fromList tyVarNames)

-- Note: gets all type variables of a field
-- e.g.
--      SomeData String a b (m c)   ->    ["a", "b", "m", "c"]
getVarNamesInType :: Type -> Set Name
getVarNamesInType (AppT s t) = getVarNamesInType s `union` getVarNamesInType t
getVarNamesInType (AppKindT _ t) = getVarNamesInType t
getVarNamesInType (SigT t _) = getVarNamesInType t
getVarNamesInType (VarT varName) = S.singleton varName
getVarNamesInType (ConT varName) = S.singleton varName
getVarNamesInType _ = empty

-- Note: replace type variable in a type
-- e.g. if
--        (toReplace, replaceWith) = ("a", "x")
--      then
--
--         MyData a b       ->   MyData x b
--         MyData (m a) b   ->   MyData (n x) b
--
replaceVarNamesInType :: Type -> (Name, Name) -> Type
replaceVarNamesInType _type i@(toReplace, replaceWith) = case _type of
  (AppT s t) -> AppT (replaceVarNamesInType s i) (replaceVarNamesInType t i)
  (AppKindT k t) -> AppKindT k $ replaceVarNamesInType t i
  (SigT t k) -> SigT (replaceVarNamesInType t i) k
  (VarT varName) -> VarT $ replace varName
  (ConT varName) -> ConT $ replace varName
  _ -> _type
  where
    replace n = if n == toReplace then replaceWith else n

replaceVarNamesInNames :: [Name] -> (Name, Name) -> [Name]
replaceVarNamesInNames names (toReplace, replaceWith) = (\n -> if n == toReplace then replaceWith else n) <$> names
