-- From "Template Meta-Programming for Haskell"
-- https://www.microsoft.com/en-us/research/publication/template-meta-programming-for-haskell
--
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}

module Template.Zip
  ( zipN,
    get1,
  )
where

import Control.Monad
import Language.Haskell.TH

get1 :: Q Exp
get1 = [|let x = 1 in $(id [|x|])|]

zipN :: Int -> Q Exp
zipN n = [|let z = $(mkZip n [|z|]) in z|]

mkZip :: Int -> Q Exp -> Q Exp
mkZip n nameExp = do
  inputNames <- replicateM n $ newName "y"
  patNames <- replicateM n $ newName "x"
  patRestNames <- replicateM n $ newName "xs"

  let casePattern = zipWith (\a b -> [p|$(varP a) : $(varP b)|]) patNames patRestNames
      zippedTuple = tupE $ varE <$> patNames
      zipRest = foldl appE nameExp $ varE <$> patRestNames
      caseExp =
        caseE
          (tupE $ varE <$> inputNames)
          [ match (tupP casePattern) (normalB [|$zippedTuple : $zipRest|]) [],
            match wildP (normalB [|[]|]) []
          ]

  lamE
    (varP <$> inputNames)
    caseExp
