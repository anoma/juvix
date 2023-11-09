module Juvix.Compiler.Pipeline.EntryPoint
  ( module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Package.Base,
  )
where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Prelude

-- | An option specifiying how symbols should be pruned in the Internal to Core translation
data SymbolPruningMode
  = FilterUnreachable
  | KeepAll
  deriving stock (Eq, Show)

-- | The head of _entryModulePaths is assumed to be the Main module
data EntryPoint = EntryPoint
  { _entryPointRoot :: Path Abs Dir,
    -- | initial root for the path resolver. Usually it should be equal to
    -- _entryPointRoot. It only differs for `juvix repl`.
    _entryPointResolverRoot :: Path Abs Dir,
    _entryPointBuildDir :: BuildDir,
    _entryPointNoTermination :: Bool,
    _entryPointNoPositivity :: Bool,
    _entryPointNoCoverage :: Bool,
    _entryPointNoStdlib :: Bool,
    _entryPointPackage :: Package,
    _entryPointPackageGlobal :: Bool,
    _entryPointStdin :: Maybe Text,
    _entryPointTarget :: Target,
    _entryPointDebug :: Bool,
    _entryPointUnsafe :: Bool,
    _entryPointUnrollLimit :: Int,
    _entryPointOptimizationLevel :: Int,
    _entryPointInliningDepth :: Int,
    _entryPointGenericOptions :: GenericOptions,
    _entryPointModulePath :: Maybe (Path Abs File),
    _entryPointSymbolPruningMode :: SymbolPruningMode,
    _entryPointOffline :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''EntryPoint

defaultEntryPoint :: Root -> Path Abs File -> EntryPoint
defaultEntryPoint root mainFile =
  (defaultEntryPointNoFile root)
    { _entryPointModulePath = Just mainFile
    }

defaultEntryPointNoFile :: Root -> EntryPoint
defaultEntryPointNoFile root =
  EntryPoint
    { _entryPointRoot = root ^. rootRootDir,
      _entryPointResolverRoot = root ^. rootRootDir,
      _entryPointBuildDir = DefaultBuildDir,
      _entryPointNoTermination = False,
      _entryPointNoPositivity = False,
      _entryPointNoCoverage = False,
      _entryPointNoStdlib = False,
      _entryPointStdin = Nothing,
      _entryPointPackage = root ^. rootPackage,
      _entryPointPackageGlobal = root ^. rootPackageGlobal,
      _entryPointGenericOptions = defaultGenericOptions,
      _entryPointTarget = TargetCore,
      _entryPointDebug = False,
      _entryPointUnsafe = False,
      _entryPointUnrollLimit = defaultUnrollLimit,
      _entryPointOptimizationLevel = defaultOptimizationLevel,
      _entryPointInliningDepth = defaultInliningDepth,
      _entryPointModulePath = Nothing,
      _entryPointSymbolPruningMode = FilterUnreachable,
      _entryPointOffline = False
    }

defaultUnrollLimit :: Int
defaultUnrollLimit = 140

defaultOptimizationLevel :: Int
defaultOptimizationLevel = 1

defaultInliningDepth :: Int
defaultInliningDepth = 3
