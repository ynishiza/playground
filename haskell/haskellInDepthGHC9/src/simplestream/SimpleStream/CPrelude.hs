{- ORMOLU_DISABLE -}
module SimpleStream.CPrelude (
  CStreamOf,
  each, 
  yield,
  toList,
  toList_,
  module X,
  ) where
{- ORMOLU_ENABLE -}

import Control.Monad
import SimpleStream.CStream as X
import SimpleStream.Of as X

type CStreamOf a = CStream (Of a)

yield :: a -> CStreamOf a m ()
yield = yieldc . (:> ())

each :: [a] -> CStreamOf a m ()
each = foldr (\a c -> yield a >> c) (pure ())

toList :: Monad m => CStream (Of a) m r -> m (Of [a] r)
toList (CStream c) =
  c
    (pure . ([] :>))
    (\(a :> m) -> mapOf (a :) <$> m)
    join

toList_ :: Monad m => CStream (Of a) m r -> m [a]
toList_ = (fst' <$>) . toList
