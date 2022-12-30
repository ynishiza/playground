{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RPC.Parse (
  remote,
  quoteDecRemote,
  genServerStub,
  genRPCTable,
  declareRPCTable,
  gen,
  test
                 ) where

import Data.ByteString (ByteString)
import Control.Monad.Catch
import Control.Monad
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Serialize
import Data.Tuple

stubName :: String
stubName = "callRemote"

decoderName :: String
decoderName = "forceDecodeParam"

instance Serialize a => Serialize (Solo a) 

remote :: QuasiQuoter
remote =
  QuasiQuoter
    { 
      quoteExp = undefined,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = quoteDecRemote
    }

quoteDecRemote :: String -> Q [Dec]
quoteDecRemote = do
  parseToFnInfo >=> (concat<$>) . traverse genClientStub

data FnInfo = MkFnInfo
  { fnName :: Name,
    fnType :: Type
  } deriving (Eq, Show)

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
  let 
    (argTypes, _) = extractTopLevelSig fnType
  argVars <- replicateM (length argTypes) $ newName "a"
  let
    argPat = varP @Q <$> argVars
    argTuple = TupE $ Just . VarE <$> argVars
    stubExpr = [| $(dyn stubName) $(litE (StringL $ nameBase fnName)) $(pure argTuple) |] :: Q Exp

  sig <- sigD fnName (pure fnType)
  dec <- funD fnName [clause argPat (normalB stubExpr) []]
  return [sig, dec]

gen :: String -> Name -> Q [Dec]
gen n1 n2 = do 
  d <- funD (mkName n1) [clause [] (normalB (genServerStub n2)) []]
  return [d]

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

genServerStubExp :: FnInfo -> Q Exp
genServerStubExp (MkFnInfo {..}) = do
  let 
    (argTypes, _) = extractTopLevelSig fnType
  argVars <- replicateM (length argTypes) $ newName "a"
  let
    callFn = foldl AppE (VarE fnName) $ VarE <$> argVars
    stubExpr = [|
        $(dyn decoderName) Stage2 
          >=> (\ $(tupP (varP <$> argVars)) -> $(pure callFn))
          >=> pure . encode
      |] :: Q Exp
  [| ($(litE $ StringL $ nameBase fnName), $stubExpr) |]

-- extract the top types of the signature.
-- In particular, the function arguments + result
-- e.g. 
--    extractTopLevelSig [t| Int -> (Maybe a) -> Double) |]
--        ==> ([Int, Maybe a], Double)
--
extractTopLevelSig :: Type -> ([Type], Type)
extractTopLevelSig (ForallT _ _ t) = extractTopLevelSig t
extractTopLevelSig (AppT (AppT ArrowT t1) (AppT (AppT ArrowT t2) t3)) = (t1:t2:c, o)
  where (c, o) = extractTopLevelSig t3
extractTopLevelSig (AppT (AppT ArrowT t1) t2) = ([t1], t2)
extractTopLevelSig t = ([], t)


test :: IO ()
test = do
  runQ [t| (Int, String, Int, Double) |] >>= print
  testParse ([t| Int -> Int |] :: Q Type) 
  testParse ([t| String -> Int -> Int |] :: Q Type)
  testParse ([t| Int -> String -> Int -> Double -> () |] :: Q Type)
  testParse ([t| forall a b c. a -> b -> c -> Int |] :: Q Type)
  testParse ([t| forall a b c. (a -> b) -> c -> Int |] :: Q Type)

  putStrLn "=========="
  runQ (quoteDecRemote "myFn :: Int -> Int -> String") >>= print
  pure ()
  where
    testParse q = do
      (tps, o) <- runQ $ extractTopLevelSig <$> q
      print (tps, o)
