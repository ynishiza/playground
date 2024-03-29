{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Lens.TH.DataConstructor
  ( genAllConstructorPrisms,
    genConstructorPrismRaw,
    genConstructorPrismExp,
  )
where

import Control.Monad
import Data.Function
import Data.Set hiding (foldl)
import Language.Haskell.TH
import Lens
import Lens.TH.Utils
import TH.Utilities

-- Note:
--
-- Creates a prism for sum types
-- e.g.
--
--      data Tree a = Leaf a | Node (Tree a) (Tree a)
--
--      _Leaf :: Prism' (Tree a) a
--      _Node :: Prism' (Tree a) (Tree a, Tree a)
--
genAllConstructorPrisms :: Dec -> Q [Dec]
genAllConstructorPrisms (DataD _ tyName tyVarBndrs _ dataCons _) =
  traverse build dataCons
    & (concat <$>)
  where
    baseTyVarNames = tyVarBndrName <$> tyVarBndrs
    build (NormalC conName conVars) =
      genConstructorPrismRaw
        (tyName, baseTyVarNames)
        (conName, snd <$> conVars)
    build (GadtC [conName] conVars ty) =
      genConstructorPrismRaw
        (tyName, n)
        (conName, snd <$> conVars)
      where
        n =
          getVarNamesInType ty
            & delete tyName
            & toList
    build (ForallC _ _ c) = build c
    build i = fail $ "Unsupported" <> show i
genAllConstructorPrisms _ = fail "Not a data constructor"

-- Note:
--
-- Creates a prism like
-- e.g. if
--
--      data Tree a = Leaf a | Node (Tree a) (Tree a)
--
--      _Leaf :: Prism' (Tree a) a
--      _Leaf = prism
--        (\s -> case s of
--          (Leaf a) -> Right a
--          _ -> Left s)
--        (a -> Leaf a)
--
genConstructorPrismRaw :: (Name, [Name]) -> (Name, [Type]) -> Q [Dec]
genConstructorPrismRaw (tyName, typeVarNames) (conName, conVars) = do
  let sType = conType tyName (VarT <$> typeVarNames)
      aType = tupleType conVars
      prismName = createPrismName conName

  prismSig <- sigD prismName [t|Prism' $(pure sType) $(pure aType)|]
  prismBody <- genConstructorPrismExp (conName, length conVars)
  return (prismSig : prismBody)

genConstructorPrismExp :: (Name, Int) -> Q [Dec]
genConstructorPrismExp (conName, conVarN) = do
  conVars <- replicateM conVarN $ newName "a"
  let valueTuple = tupleExpOf conVars
      valueTuplePat = tuplePatternOf conVars
      conExp = applyConstructorOf conName conVars
      conPat = constructorPatternOf conName conVars
      prismName = createPrismName conName
      prismExp :: Q Exp
      prismExp =
        [|
          prism
            ( \x -> case x of
                $(pure conPat) -> Right $(pure valueTuple)
                _ -> Left x
            )
            (\ $(pure valueTuplePat) -> $(pure conExp))
          |]

  dec <- funD prismName [clause [] (normalB prismExp) []]
  return [dec]

createPrismName :: Name -> Name
createPrismName conName = mkName $ "_" <> nameBase conName

tupleExpOf :: [Name] -> Exp
tupleExpOf [] = ConE br
tupleExpOf [a] = VarE a
tupleExpOf names = TupE $ Just . VarE <$> names

applyConstructorOf :: Name -> [Name] -> Exp
applyConstructorOf conName = foldl (\x r -> AppE x (VarE r)) (ConE conName)

constructorPatternOf :: Name -> [Name] -> Pat
constructorPatternOf conName varNames = ConP conName [] $ VarP <$> varNames

tuplePatternOf :: [Name] -> Pat
tuplePatternOf [] = ConP br [] []
tuplePatternOf [v] = VarP v
tuplePatternOf varNames = TupP $ VarP <$> varNames

tupleType :: [Type] -> Type
tupleType [] = ConT br
tupleType [v] = v
tupleType varNames = appsT (TupleT (length varNames)) varNames

conType :: Name -> [Type] -> Type
conType conName = appsT (ConT conName)

br :: Name
br = mkName "()"
