module MiniJuvix.Pipeline.EntryPoint where

import MiniJuvix.Prelude

-- | The head of _entryModulePaths is assumed to be the Main module.
data EntryPoint = EntryPoint
  { _entryRoot :: FilePath,
    _entryModulePaths :: NonEmpty FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''EntryPoint

mainModulePath :: Lens' EntryPoint FilePath
mainModulePath = entryModulePaths . _head
