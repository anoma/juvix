module MiniJuvix.Pipeline.EntryPoint where

import MiniJuvix.Prelude

data EntryPoint = EntryPoint
  { _entryRoot :: FilePath,
    _entryModulePaths :: NonEmpty FilePath
  }

makeLenses ''EntryPoint
