module Juvix.Compiler.Pipeline.EntryPoint
  ( module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Package.Base,
  )
where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Data.Field
import Juvix.Prelude

-- | An option specifying how symbols should be pruned in the Internal to Core translation
data SymbolPruningMode
  = FilterUnreachable
  | KeepAll
  deriving stock (Eq, Show)

-- | A module in _entryModulePath is the unit of compilation
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
    _entryPointPackageType :: PackageType,
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
    _entryPointOffline :: Bool,
    _entryPointFieldSize :: Natural
  }
  deriving stock (Eq, Show)

makeLenses ''EntryPoint

defaultEntryPoint :: Package -> Root -> Path Abs File -> EntryPoint
defaultEntryPoint pkg root mainFile =
  (defaultEntryPointNoFile pkg root)
    { _entryPointModulePath = pure mainFile
    }

defaultEntryPointNoFile :: Package -> Root -> EntryPoint
defaultEntryPointNoFile pkg root =
  EntryPoint
    { _entryPointRoot = root ^. rootRootDir,
      _entryPointResolverRoot = root ^. rootRootDir,
      _entryPointBuildDir = DefaultBuildDir,
      _entryPointNoTermination = False,
      _entryPointNoPositivity = False,
      _entryPointNoCoverage = False,
      _entryPointNoStdlib = False,
      _entryPointStdin = Nothing,
      _entryPointPackage = pkg,
      _entryPointPackageType = root ^. rootPackageType,
      _entryPointGenericOptions = defaultGenericOptions,
      _entryPointTarget = TargetCore,
      _entryPointDebug = False,
      _entryPointUnsafe = False,
      _entryPointUnrollLimit = defaultUnrollLimit,
      _entryPointOptimizationLevel = defaultOptimizationLevel,
      _entryPointInliningDepth = defaultInliningDepth,
      _entryPointModulePath = Nothing,
      _entryPointSymbolPruningMode = FilterUnreachable,
      _entryPointOffline = False,
      _entryPointFieldSize = defaultFieldSize
    }

defaultUnrollLimit :: Int
defaultUnrollLimit = 140

defaultOptimizationLevel :: Int
defaultOptimizationLevel = 1

defaultInliningDepth :: Int
defaultInliningDepth = 3
