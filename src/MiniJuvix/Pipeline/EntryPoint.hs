module MiniJuvix.Pipeline.EntryPoint
  ( module MiniJuvix.Pipeline.EntryPoint,
  )
where

import MiniJuvix.Prelude

-- | The head of _entryModulePaths is assumed to be the Main module
data EntryPoint = EntryPoint
  { _entryPointRoot :: FilePath,
    _entryPointNoTermination :: Bool,
    _entryPointModulePaths :: NonEmpty FilePath
  }
  deriving stock (Eq, Show)

defaultEntryPoint :: FilePath -> EntryPoint
defaultEntryPoint mainFile =
  EntryPoint
    { _entryPointRoot = ".",
      _entryPointNoTermination = False,
      _entryPointModulePaths = pure mainFile
    }

makeLenses ''EntryPoint

mainModulePath :: Lens' EntryPoint FilePath
mainModulePath = entryPointModulePaths . _head
