{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lens.TH.Record
  ( genAllFieldLensesForType,
    genFieldLens,
    fieldLensBody,
    mkName,
  )
where

import Control.Arrow ((>>>))
import Data.Function
import Data.Set as S hiding (foldr)
import Language.Haskell.TH
import Lens
import Lens.TH.Utils
import TH.Utilities

-- note: lens for data type
--
-- case 1: records
--   e.g. if
--
--     data MyData a b = MkMyData { v1 :: String, v2 :: a, v3 :: b, v4 :: b }
--
--        then
--
--     _v1 :: (ProfunctorArrow p, Functor f) => Optic p f (MyData a b) (MyData a b) String String         fixed field type
--     _v2 :: (ProfunctorArrow p, Functor f) => Optic p f (MyData a b) (MyData c b) a c                   polymorphic lens if there is no field overlap
--     _v3 :: (ProfunctorArrow p, Functor f) => Optic p f (MyData a b) (MyData a b) a a                   monomorphic lens if there is a field overlap
--
--
-- case 2: GADT records
--   e.g. if
--
--      data MyData a where
--        MkMyData :: { v1 :: String, v2 :: a } -> MyData a
--
--                      OR
--
--        MkMyData :: forall a. { v1 :: String, v2 :: a } -> MyData a
--
--      then
--
--      _v1 :: (ProfunctorArrow p, Functor f) => Optic p f (MyData a) (MyData a) String String
--      _v2 :: (ProfunctorArrow p, Functor f) => Optic p f (MyData a) (MyData b) a b
--
genAllFieldLensesForType :: Dec -> Q [Dec]
genAllFieldLensesForType (DataD _ tyName tyVarBndrs _ dataCons _) =
  traverse buildForDataConstructor dataCons
    & (concat <$>)
  where
    baseTyVarNames = tyVarBndrName <$> tyVarBndrs
    buildForDataConstructor :: Con -> Q [Dec]
    buildForDataConstructor (RecC _ fieldVars) = buildFieldVars baseTyVarNames fieldVars
    buildForDataConstructor (RecGadtC _ fieldVars ty) =
      buildFieldVars
        -- get type variables except itself
        ( getVarNamesInType ty
            & delete tyName
            & toList
        )
        fieldVars
    buildForDataConstructor (ForallC _ _ c) = buildForDataConstructor c
    buildForDataConstructor x = fail $ "Unsupported type:" <> show x
    buildFieldVars :: [Name] -> [VarBangType] -> Q [Dec]
    buildFieldVars tv fieldVars =
      traverse
        ( \(fieldName, _, fieldTy) ->
            genFieldLens
              ((tyName, tv), (fieldName, fieldTy))
              (isPolymorphicLens fieldName fieldTy)
        )
        fieldVars
        & (concat <$>)
      where
        otherFields name =
          Prelude.filter (hasn't (_1 . only name)) fieldVars
            & toListOf (traverse . _3)
        -- note: lens may be polymorphic only if there is no field overlap
        isPolymorphicLens name ty = not $ hasVariableOverlapWith ty $ otherFields name
genAllFieldLensesForType _ = fail "Not a data constructor"

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
genFieldLens :: (TypeConInfo, FieldInfo) -> Bool -> Q [Dec]
genFieldLens ((tyName, tyVarNames), (fieldName, fieldTy)) isPolymorphic = do
  (mappedTypeVars, mappedFieldType) <-
    if isPolymorphic
      then assignNewTypeVariables (tyVarNames, fieldTy)
      else return (tyVarNames, fieldTy)
  pType <- newName "p"
  fType <- newName "f"
  let srcType = appsT (ConT tyName) (VarT <$> tyVarNames)
      dstType = appsT (ConT tyName) (VarT <$> mappedTypeVars)
  sigType <-
    [t|
      (ProfunctorArrow $(varT pType), Functor $(varT fType)) =>
      Optic $(varT pType) $(varT fType) $(pure srcType) $(pure dstType) $(pure fieldTy) $(pure mappedFieldType)
      |]

  lensSignature <- sigD (createLensName fieldName) (pure sigType)
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
