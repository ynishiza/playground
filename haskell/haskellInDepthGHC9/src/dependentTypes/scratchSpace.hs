{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.Kind
import GHC.TypeLits

type MyLabel :: forall a -> Constraint
class MyLabel (a :: k)  where
  myLabel :: String
instance MyLabel String where myLabel = "S"
instance MyLabel Maybe where myLabel = "Maybe"
instance (MyLabel l) => MyLabel (m l) where myLabel = "(" ++ myLabel @l ++ ")"
instance MyLabel Int where myLabel = "I"

data Answer = YES | NO
class HasSymbol s where
  getSymbol :: Answer
instance HasSymbol 'YES where getSymbol = YES
instance HasSymbol 'NO where getSymbol = NO

data Key = A | B | C | D
data Mod (a :: Key) = Bold | Italic | Underline
