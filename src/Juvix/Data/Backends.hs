module Juvix.Data.Backends where

import Juvix.Data.WithLoc
import Juvix.Prelude.Base

data Backend
  = BackendGhc
  | BackendC
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable Backend

data BackendItem = BackendItem
  { _backendItemBackend :: WithLoc Backend,
    _backendItemCode :: Text
  }
  deriving stock (Show, Ord, Eq, Generic, Data)

instance Hashable BackendItem

makeLenses ''BackendItem
