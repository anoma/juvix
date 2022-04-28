module MiniJuvix.Syntax.Backends where

import MiniJuvix.Prelude

data Backend = BackendGhc | BackendAgda
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Backend

data BackendItem = BackendItem
  { _backendItemBackend :: Backend,
    _backendItemCode :: Text
  }
  deriving stock (Show, Ord, Eq, Generic)

instance Hashable BackendItem

makeLenses ''BackendItem

isBackendSupported :: Backend -> Bool
isBackendSupported BackendGhc = True
isBackendSupported BackendAgda = False
