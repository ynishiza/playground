{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}

module List
  ( test,
    List (..),
    ListF,
    liftNil,
    liftCons,
    liftList,
    execListF,
  )
where

import Data.Foldable
import Data.Kind
import Free

type List :: Type -> Type -> Type
data List t a where
  Nil :: a -> List t a
  Cons :: t -> a -> List t a
  deriving (Functor)

type ListF t = F (List t)

liftNil :: ListF t ()
liftNil = liftF $ Nil ()

liftCons :: t -> ListF t ()
liftCons t = liftF $ Cons t ()

liftList :: [t] -> ListF t ()
liftList = foldr (\t r -> liftCons t >> r) liftNil

execListF :: ListF t a -> ([t], a)
execListF (F f) = f ([],) m
  where
    m :: List t ([t], r) -> ([t], r)
    m (Nil (_, r)) = ([], r)
    m (Cons t (ts, a)) = (t : ts, a)

test :: IO ()
test = do
  let p0 :: ListF Int Int
      p0 = liftPure 100

      l0 :: ListF Int ()
      l0 = liftNil

      l1 :: ListF Int ()
      l1 = do
        liftCons 1
        liftCons 2
        liftCons 3

      l2 :: ListF Int ()
      l2 = do
        liftCons 1
        liftNil
        liftNil
        liftCons 2

      l3 :: ListF Int ()
      l3 = do
        liftCons 1
        liftList [100 .. 105]

  putStrLn $ "p0:" <> show (execListF p0)
  putStrLn $ "l0:" <> show (execListF l0)
  putStrLn $ "l1:" <> show (execListF l1)
  putStrLn $ "l2:" <> show (execListF l2)
  putStrLn $ "l3:" <> show (execListF l3)

  putStrLn "==================== cutoff ===================="
  traverse_
    ( \n ->
        putStrLn ("cutoff l3 " <> show n <> ":" <> show (execListF $ cutoff' n l3))
    )
    [1 .. 10]
  traverse_
    ( \n ->
        putStrLn ("cutoff l3 " <> show n <> ":" <> show (execListF $ cutoff n l3))
    )
    [1 .. 10]

  putStrLn $ "cutoff1 p0" <> show (execListF $ cutoff0 p0)
  putStrLn $ "cutoff1 l0" <> show (execListF $ cutoff0 l0)
  putStrLn $ "cutoff1 l1" <> show (execListF $ cutoff0 l1)

  pure ()
