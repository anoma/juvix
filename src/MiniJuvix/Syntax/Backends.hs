module MiniJuvix.Syntax.Backends where

import MiniJuvix.Prelude

data Backend = BackendGhc | BackendAgda
  deriving stock (Show, Eq, Ord)

data BackendItem = BackendItem
  { _backendItemBackend :: Backend,
    _backendItemCode :: Text
  }
  deriving stock (Show, Eq, Ord)

makeLenses ''BackendItem
