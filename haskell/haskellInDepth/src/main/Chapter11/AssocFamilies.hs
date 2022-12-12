{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Chapter11.AssocFamilies (run) where

import Data.List (nub)
import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "11.3.4"
    "AssocFamilies"
    ( do
        let l :: [(Int, Int)]
            l = filter (uncurry (<)) $ (,) <$> [1 .. 10] <*> [1 .. 10]
            g :: EdgeList Int
            g = EdgeList $ map (uncurry MyEdge) l
            v1 = 3 :: Int
            e1 = MyEdge v1 v1
         in do
              fmtLn $
                nameF ("outEdges:" +|| v1 ||+ "") (showBuilder $ outEdges g v1)
                  <> nameF ("inEdges:" +|| v1 ||+ "") (showBuilder $ inEdges g v1)
                  <> nameF ("neighborVertices:" +|| v1 ||+ "") (showBuilder $ neighborVertices g v1)
                  <> nameF ("isLoopEdge:" +|| e1 ||+ "") (showBuilder $ isLoopEdge e1)

              let 
                x :: [IntEdge]
                x = soutEdges g v1
              fmtLn $
                nameF ("outEdges:" +|| v1 ||+ "") (showBuilder $ (soutEdges g v1 :: [IntEdge]))
        testDone
    )

class Graph g where
  type Vertex g
  data Edge g
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]
  inEdges :: g -> Vertex g -> [Edge g]

type MyVertex a = a

newtype EdgeList a = EdgeList [Edge (EdgeList a)]

type IntEdge = Edge (EdgeList Int)

instance Eq a => Graph (EdgeList a) where
  type Vertex (EdgeList a) = MyVertex a
  data Edge (EdgeList a) = MyEdge (Vertex (EdgeList a)) (Vertex (EdgeList a))
    deriving (Show, Eq)
  src (MyEdge v _) = v
  tgt (MyEdge _ v) = v
  outEdges (EdgeList l) v = filter ((== v) . src) l
  inEdges (EdgeList l) v = filter ((== v) . tgt) l

neighborVertices :: (Eq (Vertex g), Graph g) => g -> Vertex g -> [Vertex g]
neighborVertices g v =
  nub $ map src (outEdges g v) <> map src (inEdges g v)

isLoopEdge :: (Eq (Vertex g), Graph g) => Edge g -> Bool
isLoopEdge e = src e == tgt e

class SGraph g e v where
  ssrc, stgt :: e -> v
  soutEdges :: g -> v -> [e]
  sinEdges :: g -> v -> [e]

instance (Eq a) => SGraph (EdgeList a) (Edge (EdgeList a)) (MyVertex a) where
  ssrc (MyEdge v _) = v
  stgt (MyEdge _ v) = v
  soutEdges (EdgeList l) v = filter ((== v) . src) l
  sinEdges (EdgeList l) v = filter ((== v) . tgt) l
