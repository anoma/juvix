module Juvix.Compiler.Pipeline.EntryPoint
  ( module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Package,
  )
where

import Juvix.Compiler.Pipeline.Package
import Juvix.Prelude

-- | The head of _entryModulePaths is assumed to be the Main module
data EntryPoint = EntryPoint
  { _entryPointRoot :: FilePath,
    _entryPointNoTermination :: Bool,
    _entryPointNoPositivity :: Bool,
    _entryPointNoStdlib :: Bool,
    _entryPointPackage :: Package,
    _entryPointStdin :: Maybe Text,
    _entryPointModulePaths :: NonEmpty FilePath
  }
  deriving stock (Eq, Show)

defaultEntryPoint :: FilePath -> EntryPoint
defaultEntryPoint mainFile =
  EntryPoint
    { _entryPointRoot = ".",
      _entryPointNoTermination = False,
      _entryPointNoPositivity = False,
      _entryPointNoStdlib = False,
      _entryPointStdin = Nothing,
      _entryPointPackage = emptyPackage,
      _entryPointModulePaths = pure mainFile
    }

makeLenses ''EntryPoint

mainModulePath :: Lens' EntryPoint FilePath
mainModulePath = entryPointModulePaths . _head
