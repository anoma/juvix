module Juvix.Data.Uid where

import Data.Type.Equality
import Juvix.Prelude.Base
import Type.Reflection qualified as R

data Uid = forall a.
  (Typeable a, Hashable a, Eq a) =>
  Uid
  { _fileUid :: a
  }

instance Eq Uid where
  Uid a == Uid b = case testEquality (R.typeOf a) (R.typeOf b) of
    Nothing -> False
    Just Refl -> a == b

instance Hashable Uid where
  hashWithSalt s (Uid u) = hashWithSalt s u
