module Juvix.Syntax.Backends where

import Juvix.Prelude

data Backend = BackendGhc | BackendC
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Backend

data BackendItem = BackendItem
  { _backendItemBackend :: Backend,
    _backendItemCode :: Text
  }
  deriving stock (Show, Ord, Eq, Generic)

instance Hashable BackendItem

makeLenses ''BackendItem
