{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

import Data.Singletons.TH (genSingletons)

genSingletons [''Either]

data MyEither a b where
  ML :: a -> MyEither a b
  MR :: b -> MyEither a b

