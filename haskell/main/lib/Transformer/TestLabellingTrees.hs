---- NOTE: labelling tree example
--
-- From the official documentation: https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-State-Lazy.html#g:8
--
module Transformer.TestLabellingTrees
  ( testTreeToNumber,
  )
where

import Control.Monad.Trans.State.Lazy
import Data.List (elemIndex, intercalate)
import TestUtils

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

toNumberTree :: Eq a => Tree a -> State [a] (Tree Int)
toNumberTree tree
  | (Leaf value) <- tree = do
      n <- getNumber value
      return (Leaf n)
  | (Node value tree1 tree2) <- tree = do
      n <- getNumber value
      ntree1 <- toNumberTree tree1
      ntree2 <- toNumberTree tree2
      return (Node n ntree1 ntree2)
  where
    getNumber :: Eq a => a -> State [a] Int
    getNumber value = do
      values <- get
      case elemIndex value values of
        (Just i) -> return i
        _ -> do
          put (values ++ [value])
          return $ length values

toNumberTreeRecursive :: Eq a => ([a], Tree a) -> ([a], Tree Int)
toNumberTreeRecursive (values, tree)
  | (Leaf value) <- tree =
      let (_values, n) = getValues values value
       in (_values, Leaf n)
  | otherwise =
      let (Node value tree1 tree2) = tree
          (_values1, _tree1) = toNumberTreeRecursive (values, tree1)
          (_values2, _tree2) = toNumberTreeRecursive (_values1, tree2)
          (_values3, n) = getValues _values2 value
       in (_values3, Node n _tree1 _tree2)
  where
    getValues :: Eq a => [a] -> a -> ([a], Int)
    getValues _values v =
      case elemIndex v _values of
        (Just i) -> (_values, i)
        _ -> (_values ++ [v], length _values)

testTreeToNumber :: TestState
testTreeToNumber =
  createTest
    ( do
        let leafa = Leaf 'a'
            leafb = Leaf 'b'
            leafc = Leaf 'c'
            leafd = Leaf 'd'
            leafe = Leaf 'e'
            leaff = Leaf 'f'
            treeA = Node 'A' leafa leafb
            treeB = Node 'B' leafb leafc
            treeC = Node 'C' leafc leafd
            treeD = Node 'D' leafd leafe
            treeE = Node 'E' leafe leaff
            tree1 = Node 'A' treeA (Node 'B' treeB treeC)
            tree2 = Node 'B' treeB (Node 'C' treeB treeC)

            printTrees :: (Show a, Eq a) => [Tree a] -> State (IO (), [a]) ()
            printTrees [] = pure ()
            printTrees (tree : rest) = do
              (io, values) <- get
              let (itree, values') = runState (toNumberTree tree) values
                  (values_rec, itree_rec) = toNumberTreeRecursive (values, tree)
                  io' = do
                    io
                    putStrLn ""
                    putStrLn $ "tree:" ++ show tree
                    putStrLn $ "numbered:" ++ show itree
                    putStrLn $ "table:" ++ printValues values'
                    putStrLn $ "numbered (recursive):" ++ show itree_rec
                    putStrLn $ "table (recursive):" ++ printValues values_rec
                    assertIsEqual itree itree_rec
              put (io', values')
              printTrees rest
              where
                printValues values =
                  intercalate
                    "  "
                    ( do
                        (v, i) <- zip values [0, 1 ..]
                        return $ show v ++ "=>" ++ show i
                    )

        let trees =
              [ leafa,
                leafb,
                leafc,
                leafd,
                leafe,
                leaff,
                treeA,
                treeB,
                treeC,
                treeD,
                treeE,
                tree1,
                tree2
              ]
            result = execState (printTrees trees) (pure (), [])
         in fst result
        testDone
    )
    "testTreeToNumber"
