{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RPC.Parse
  ( remote,
    quoteDecRemote,
    genServerStub,
    genRPCTable,
    declareRPCTable,
    test,
    R,
  )
where

import Data.List (isSuffixOf)
import Data.Serialize
import Data.Tuple
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import RPC.Common
import RPC.ParseUtils

stubName :: String
stubName = "callRemote"

decoderName :: String
decoderName = "forceDecodeParam"

instance Serialize a => Serialize (Solo a)

remote :: QuasiQuoter
remote =
  QuasiQuoter
    { quoteExp = undefined,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = quoteDecRemote
    }

quoteDecRemote :: String -> Q [Dec]
quoteDecRemote = do
  parseToFnInfo >=> (concat <$>) . traverse genClientStub

data FnInfo = MkFnInfo
  { fnName :: Name,
    fnType :: Type
  }
  deriving (Eq, Show)

parseToFnInfo :: MonadThrow m => String -> m [FnInfo]
parseToFnInfo s =
  case parseDecs s of
    (Right decs) -> pure $ decToFn <$> decs
    (Left e) -> error e

decToFn :: Dec -> FnInfo
decToFn (SigD fnName fnType) = MkFnInfo fnName fnType
decToFn _ = error "Unsupported"

genClientStub :: FnInfo -> Q [Dec]
genClientStub (MkFnInfo {..}) = do
  let (argTypes, o) = extractTopLevelSig fnType
  requireRSIO o
  argVars <- replicateM (length argTypes) $ newName "a"
  let argPat = varP @Q <$> argVars
      argTuple = TupE $ Just . VarE <$> argVars
      stubExpr = [|$(dyn stubName) $(litE (StringL $ nameBase fnName)) $(pure argTuple)|] :: Q Exp

  sig <- sigD fnName (pure fnType)
  dec <- funD fnName [clause argPat (normalB stubExpr) []]
  return [sig, dec]

declareRPCTable :: String -> [Name] -> Q [Dec]
declareRPCTable fn nms = do
  d <- funD (mkName fn) [clause [] (normalB $ genRPCTable nms) []]
  return [d]

genRPCTable :: [Name] -> Q Exp
genRPCTable = (ListE <$>) . traverse genServerStub

genServerStub :: Name -> Q Exp
genServerStub n = do
  (VarI _ t Nothing) <- reify n
  genServerStubExp $ MkFnInfo n t

requireRSIO :: Type -> Q ()
requireRSIO t = isRSIO t >>= (`unless` failOnTypeError "Output must be RSIO" t)

isRSIO :: Type -> Q Bool
isRSIO t = do
  resolved <- resolveSynonym t
  case resolved of
    (ConT name) -> pure $ "RSIO" `isSuffixOf` nameBase name
    _ -> failOnTypeError ("Failed to resolve resolved:" +|| resolved ||+ "") t

genServerStubExp :: FnInfo -> Q Exp
genServerStubExp (MkFnInfo {..}) = do
  let (argTypes, o) = extractTopLevelSig fnType
  requireRSIO o
  argVars <- replicateM (length argTypes) $ newName "a"
  let callFn = foldl AppE (VarE fnName) $ VarE <$> argVars
      stubExpr =
        [|
          $(dyn decoderName) Stage2
            >=> (\ $(tupP (varP <$> argVars)) -> $(pure callFn))
            >=> pure . encode
          |] ::
          Q Exp
  [|($(litE $ StringL $ nameBase fnName), $stubExpr)|]

type R = RSIO () Int

test :: IO ()
test = do
  runQ [t|(Int, String, Int, Double)|] >>= print
  testParse ([t|Int -> Int|] :: Q Type)
  testParse ([t|String -> Int -> Int|] :: Q Type)
  testParse ([t|Int -> String -> Int -> Double -> ()|] :: Q Type)
  testParse ([t|forall a b c. a -> b -> c -> Int|] :: Q Type)
  testParse ([t|forall a b c. (a -> b) -> c -> Int|] :: Q Type)

  putStrLn "=========="
  runQ (quoteDecRemote "myFn :: Int -> Int -> String") >>= print
  pure ()
  where
    testParse q = do
      (tps, o) <- runQ $ extractTopLevelSig <$> q
      print (tps, o)
