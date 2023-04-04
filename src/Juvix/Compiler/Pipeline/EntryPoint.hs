module Juvix.Compiler.Pipeline.EntryPoint
  ( module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Package,
  )
where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.Package
import Juvix.Extra.Paths
import Juvix.Prelude

-- | The head of _entryModulePaths is assumed to be the Main module
data EntryPoint = EntryPoint
  { _entryPointRoot :: Path Abs Dir,
    -- | initial root for the path resolver. Usually it should be equal to
    -- _entryPointRoot. It only differs for `juvix repl`.
    _entryPointResolverRoot :: Path Abs Dir,
    _entryPointBuildDir :: SomeBase Dir,
    _entryPointNoTermination :: Bool,
    _entryPointNoPositivity :: Bool,
    _entryPointNoCoverage :: Bool,
    _entryPointNoStdlib :: Bool,
    _entryPointPackage :: Package,
    _entryPointPackageGlobal :: Bool,
    _entryPointStdin :: Maybe Text,
    _entryPointTarget :: Target,
    _entryPointDebug :: Bool,
    _entryPointUnrollLimit :: Int,
    _entryPointGenericOptions :: GenericOptions,
    _entryPointModulePaths :: NonEmpty (Path Abs File)
  }
  deriving stock (Eq, Show)

makeLenses ''EntryPoint

defaultEntryPoint :: (Package, Bool) -> Path Abs Dir -> Path Abs File -> EntryPoint
defaultEntryPoint (pkg, global) root mainFile =
  EntryPoint
    { _entryPointRoot = root,
      _entryPointResolverRoot = root,
      _entryPointBuildDir = Rel relBuildDir,
      _entryPointNoTermination = False,
      _entryPointNoPositivity = False,
      _entryPointNoCoverage = False,
      _entryPointNoStdlib = False,
      _entryPointStdin = Nothing,
      _entryPointPackage = pkg,
      _entryPointPackageGlobal = global,
      _entryPointGenericOptions = defaultGenericOptions,
      _entryPointTarget = TargetCore,
      _entryPointDebug = False,
      _entryPointUnrollLimit = defaultUnrollLimit,
      _entryPointModulePaths = pure mainFile
    }

defaultUnrollLimit :: Int
defaultUnrollLimit = 140

mainModulePath :: Lens' EntryPoint (Path Abs File)
mainModulePath = entryPointModulePaths . _head1
