module Chapter11.SimpleGraph
  ( SimpleEdge (..),
    SimpleGraph (..),
  )
where

class SimpleEdge e v => SimpleGraph g e v where
  soutEdges :: g -> v -> [e]
  sinEdges :: g -> v -> [e]

class SimpleEdge e v where
  ssrc, stgt :: e -> v
