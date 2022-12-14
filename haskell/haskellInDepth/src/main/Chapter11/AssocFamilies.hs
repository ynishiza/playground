-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Chapter11.AssocFamilies (run) where

import Data.List (nub, sort)
import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "11.3.4"
    "AssocFamilies"
    ( do
        let edges :: [(Int, Int)]
            edges =
              filter (uncurry (<)) $
                (,) <$> [1 .. 10] <*> [1 .. 10]
            grph :: GGraph Int
            grph = GGraph $ map (uncurry GEdge) edges
            v1 = 3 :: Int
            e1 = GEdge v1 v1
            eLoop = GEdge v1 10
            wrapText :: Show a => String -> a -> Builder
            wrapText label v = label |+ " [" +|| v ||+ "]"
         in do
              printBanner "Graph with type family"
              fmtLn $
                nameF "graph" (showBuilder grph)
                  <> nameF (wrapText "outEdges" v1) (showBuilder $ outEdges grph v1)
                  <> nameF (wrapText "inEdges" v1) (showBuilder $ inEdges grph v1)
                  <> nameF (wrapText "neighborVertices" v1) (showBuilder $ neighborVertices grph v1)
                  <> nameF (wrapText "isLoopEdge" eLoop) (showBuilder $ isLoopEdge eLoop)

              assertIsEqual (outEdges grph v1) (GEdge 3 <$> [4 .. 10])
              assertIsEqual (inEdges grph v1) (flip GEdge 3 <$> [1, 2])
              assertIsEqual (neighborVertices grph v1) ([1, 2] ++ [4 .. 10])

              printBanner "Graph without type family"
              fmtLn $
                nameF (wrapText "soutEdges" v1) (showBuilder (soutEdges grph v1 :: [IntEdge]))
                  <> nameF (wrapText "sinEdges" v1) (showBuilder (sinEdges grph v1 :: [IntEdge]))
              assertIsEqual (src e1) (ssrc e1)
              assertIsEqual (tgt e1) (stgt e1)
              assertIsEqual (soutEdges grph v1) (outEdges grph v1)
              assertIsEqual (sinEdges grph v1) (inEdges grph v1)
        testDone
    )

-- case: graph with type families
class Graph g where
  type Vertex g
  data Edge g
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]
  inEdges :: g -> Vertex g -> [Edge g]

-- case: graph without type families
class SimpleEdge e v => SimpleGraph g e v where
  soutEdges :: g -> v -> [e]
  sinEdges :: g -> v -> [e]

class SimpleEdge e v where
  ssrc, stgt :: e -> v

neighborVertices :: (Ord (Vertex g), Graph g) => g -> Vertex g -> [Vertex g]
neighborVertices g v =
  sort $ nub $ map tgt (outEdges g v) <> map src (inEdges g v)

isLoopEdge :: (Eq (Vertex g), Graph g) => Edge g -> Bool
isLoopEdge e = src e == tgt e

newtype GGraph a = GGraph [Edge (GGraph a)] deriving (Show, Eq)

type IntEdge = Edge (GGraph Int)

instance Ord a => Graph (GGraph a) where
  type Vertex (GGraph a) = a
  data Edge (GGraph a) = GEdge (Vertex (GGraph a)) (Vertex (GGraph a))
    deriving (Show, Eq, Ord)
  src (GEdge v _) = v
  tgt (GEdge _ v) = v
  outEdges (GGraph l) v = sort $ filter ((== v) . src) l
  inEdges (GGraph l) v = sort $ filter ((== v) . tgt) l

instance SimpleEdge (Edge (GGraph a)) a where
  ssrc (GEdge v _) = v
  stgt (GEdge _ v) = v

instance (Ord a) => SimpleGraph (GGraph a) (Edge (GGraph a)) a where
  soutEdges g v = outEdges g v
  sinEdges g v = inEdges g v
