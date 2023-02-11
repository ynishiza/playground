{- ORMOLU_DISABLE -}
module SimpleStream.CPrelude (
  CStreamOf,
  each, 
  yield,
  toList,
  module X,
  ) where
{- ORMOLU_ENABLE -}

import Control.Monad
import SimpleStream.CStream
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
