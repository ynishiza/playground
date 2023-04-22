{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lens.TH (createFieldLenses, fieldLensBody, mkName) where

import Control.Arrow ((>>>))
import Control.Monad
import Data.Function
import Data.Set as S hiding (foldr)
import Language.Haskell.TH
import Lens.Class
import Lens.Lens
import TH.Utilities

createFieldLenses :: Name -> Q [Dec]
createFieldLenses =
  reify
    >=> ( \case
            TyConI d -> createFieldLensesForDec d
            info -> fail $ "Not a type constructor:" <> show info
        )

createFieldLensesForDec :: Dec -> Q [Dec]
-- note: lens for data type
--
-- case 1: records
--   e.g.
--      data MyData a = MkMyData { v1 :: String, v2 :: a }
--
-- case 2: GADT records
--   e.g.
--      data MyData a where
--        MkMyData :: { v1 :: String, v2 :: a } -> MyData a
--        MkMyData :: forall a. { v1 :: String, v2 :: a } -> MyData a
--
--
createFieldLensesForDec (DataD _ tyName tyVarBndrs _ dataCons _) =
  traverse (buildForDataConstructor (tyVarBndrName <$> tyVarBndrs)) dataCons
    & (concat <$>)
  where
    buildForDataConstructor tyVarNames (RecC _ fieldVars) = buildFieldVars tyVarNames fieldVars
    buildForDataConstructor tyVarNames (RecGadtC _ fieldVars _) = buildFieldVars tyVarNames fieldVars
    buildForDataConstructor _ (ForallC t _ c) = buildForDataConstructor (tyVarBndrName <$> t) c
    buildForDataConstructor _ x = fail $ "Unsupported type:" <> show x
    buildFieldVars tyVarNames fieldVars =
      traverse
        ( \(fieldName, _, fieldTy) ->
            createFieldLens ((tyName, tyVarNames), (fieldName, fieldTy))
        )
        fieldVars
        & (concat <$>)
createFieldLensesForDec _ = fail "Not a data constructor"

-- note: create lens for any data field
--
--  e.g. if
--      data MyData a = MyData { getA :: a }
--
--    then build lens _getA by
--
--      _getA :: (ProfunctorArrow p, Functor f) => Optic p f (MyData a) (MyData b) a b
--      _getA = lmap getA
--        >>> strong (\s fb -> (\b -> s { getA = b }) <$> fb)
--
createFieldLens :: (TypeConInfo, FieldInfo) -> Q [Dec]
createFieldLens info@((tyName, tyVarNames), (fieldName, fieldTy)) = do
  (mappedTypeVars, mappedFieldType) <- assignNewFieldVars info
  pType <- newName "p"
  fType <- newName "f"
  let srcType = appsT (ConT tyName) (VarT <$> tyVarNames)
      dstType = appsT (ConT tyName) (VarT <$> mappedTypeVars)
      sigType :: Q Type
      sigType =
        [t|
          (ProfunctorArrow $(varT pType), Functor $(varT fType)) =>
          Optic $(varT pType) $(varT fType) $(pure srcType) $(pure dstType) $(pure fieldTy) $(pure mappedFieldType)
          |]

  lensSignature <- sigD (createLensName fieldName) sigType
  lensExpression <- fieldLensBody fieldName
  return (lensSignature : lensExpression)

createLensName :: Name -> Name
createLensName name = mkName $ "_" <> nameBase name

fieldLensBody :: Name -> Q [Dec]
fieldLensBody fieldName = do
  srcValue <- newName "src"
  x <- newName "x"
  let -- e.g.
      --      src { getA = x }
      updateFieldExp :: Q Exp
      updateFieldExp = recUpdE (varE srcValue) [pure (fieldName, VarE x)]
      lensName = createLensName fieldName
      updateLens =
        [|
          lmap $(varE fieldName)
            >>> strong (\ $(varP srcValue) v -> (\ $(varP x) -> $(updateFieldExp)) <$> v)
          |]

  (: []) <$> funD lensName [clause [] (normalB updateLens) []]

type TypeConInfo = (Name, [Name])

type FieldInfo = (Name, Type)

assignNewFieldVars :: (TypeConInfo, FieldInfo) -> Q ([Name], Type)
assignNewFieldVars ((_, tyVarNames), (_, fieldTy)) = do
  newVarNames <- replicateM (size toReplace) $ newName "a"
  let oldNew = zip (S.toList toReplace) newVarNames
      newFieldTy = foldr (flip replaceTypeVarsInType) fieldTy oldNew
      newTyVarNames = foldr (flip replaceTypeVarsInNames) tyVarNames oldNew
  return (newTyVarNames, newFieldTy)
  where
    fieldVars = getTypeVars fieldTy
    toReplace = intersection fieldVars (S.fromList tyVarNames)

-- Note: gets all type variables of a field
-- e.g.
--      SomeData String a b (m c)   ->    ["a", "b", "m", "c"]
getTypeVars :: Type -> Set Name
getTypeVars (AppT s t) = getTypeVars s `union` getTypeVars t
getTypeVars (AppKindT _ t) = getTypeVars t
getTypeVars (SigT t _) = getTypeVars t
getTypeVars (VarT varName) = S.singleton varName
getTypeVars (ConT varName) = S.singleton varName
getTypeVars _ = empty

-- Note: replace type variable in a type
-- e.g. if
--        (toReplace, replaceWith) = ("a", "x")
--      then
--
--         MyData a b       ->   MyData x b
--         MyData (m a) b   ->   MyData (m x) b
--
replaceTypeVarsInType :: Type -> (Name, Name) -> Type
replaceTypeVarsInType _type i@(toReplace, replaceWith) = case _type of
  (AppT s t) -> AppT (replaceTypeVarsInType s i) (replaceTypeVarsInType t i)
  (AppKindT k t) -> AppKindT k $ replaceTypeVarsInType t i
  (SigT t k) -> SigT (replaceTypeVarsInType t i) k
  (VarT varName) -> VarT $ replace varName
  (ConT varName) -> ConT $ replace varName
  _ -> _type
  where
    replace n = if n == toReplace then replaceWith else n

replaceTypeVarsInNames :: [Name] -> (Name, Name) -> [Name]
replaceTypeVarsInNames names (toReplace, replaceWith) = (\n -> if n == toReplace then replaceWith else n) <$> names
