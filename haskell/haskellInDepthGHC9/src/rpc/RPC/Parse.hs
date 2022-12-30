{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RPC.Parse (
  remote,
  quoteDecRemote,
  test
                 ) where

import Control.Monad.Catch
import Control.Monad
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Serialize
import Data.Tuple

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
  parseToFnInfo >=> (concat<$>) . traverse generateStubCaller

data FnInfo = MkFnInfo
  { fnName :: Name,
    fnType :: Type
  } deriving (Eq, Show)

parseToFnInfo :: MonadThrow m => String -> m [FnInfo]
parseToFnInfo s = 
  case parseDecs s of
    (Right decs) -> pure $ decToFn <$> decs
    (Left e) -> error e

generateStubCaller :: FnInfo -> Q [Dec]
generateStubCaller (MkFnInfo {..}) = do
  let 
    (inTs, _) = parseSig fnType
  vars <- replicateM (length inTs) $ newName "a"
  let
    varsP = varP @Q <$> vars
    tup = TupE $ Just . VarE <$> vars
    expr = [| $(dyn "callRemote") $(litE (StringL $ nameBase fnName)) $(pure tup) |] :: Q Exp
    cl = clause varsP (normalB expr) []

  sig <- sigD fnName (pure fnType)
  -- undefined
  dec <- funD fnName [cl]
  return [sig, dec]
  -- return [dec]

decToFn :: Dec -> FnInfo
decToFn (SigD fnName fnType) = MkFnInfo fnName fnType
decToFn _ = error "Unsupported"

toTuple :: [Type] -> Type
toTuple l = foldl AppT (TupleT (length l)) l

parseSig :: Type -> ([Type], Type)
parseSig (ForallT _ _ t) = parseSig t
parseSig (AppT (AppT ArrowT t1) (AppT (AppT ArrowT t2) t3)) = (t1:t2:c, o)
  where (c, o) = parseSig t3
parseSig (AppT (AppT ArrowT t1) t2) = ([t1], t2)
parseSig t = ([], t)


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
      (tps, o) <- runQ $ parseSig <$> q
      print (tps, o)
      print $ toTuple tps

