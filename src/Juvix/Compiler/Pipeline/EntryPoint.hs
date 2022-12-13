module Juvix.Compiler.Pipeline.EntryPoint
  ( module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Package,
  )
where

import Juvix.Compiler.Pipeline.Package
import Juvix.Prelude

-- | The head of _entryModulePaths is assumed to be the Main module
data EntryPoint = EntryPoint
  { _entryPointRoot :: Path Abs Dir,
    _entryPointNoTermination :: Bool,
    _entryPointNoPositivity :: Bool,
    _entryPointNoStdlib :: Bool,
    _entryPointStdlibPath :: Maybe (Path Abs Dir),
    _entryPointPackage :: Package,
    _entryPointStdin :: Maybe Text,
    _entryPointGenericOptions :: GenericOptions,
    _entryPointModulePaths :: NonEmpty (Path Abs File)
  }
  deriving stock (Eq, Show)

makeLenses ''EntryPoint

defaultEntryPoint :: Path Abs Dir -> Path Abs File -> EntryPoint
defaultEntryPoint root mainFile =
  EntryPoint
    { _entryPointRoot = root,
      _entryPointNoTermination = False,
      _entryPointNoPositivity = False,
      _entryPointNoStdlib = False,
      _entryPointStdlibPath = Nothing,
      _entryPointStdin = Nothing,
      _entryPointPackage = defaultPackage root,
      _entryPointGenericOptions = defaultGenericOptions,
      _entryPointModulePaths = pure mainFile
    }

mainModulePath :: Lens' EntryPoint (Path Abs File)
mainModulePath = entryPointModulePaths . _head1
