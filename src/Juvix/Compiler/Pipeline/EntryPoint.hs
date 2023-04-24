module Juvix.Compiler.Pipeline.EntryPoint
  ( module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Package,
  )
where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Root
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

defaultEntryPointCwdIO :: Path Abs File -> IO EntryPoint
defaultEntryPointCwdIO mainFile = do
  cwd <- getCurrentDir
  roots <- findRootAndChangeDir (Just (parent mainFile)) Nothing cwd
  return (defaultEntryPoint roots mainFile)

defaultEntryPoint :: Roots -> Path Abs File -> EntryPoint
defaultEntryPoint roots mainFile =
  EntryPoint
    { _entryPointRoot = roots ^. rootsRootDir,
      _entryPointResolverRoot = roots ^. rootsRootDir,
      _entryPointBuildDir = Rel relBuildDir,
      _entryPointNoTermination = False,
      _entryPointNoPositivity = False,
      _entryPointNoCoverage = False,
      _entryPointNoStdlib = False,
      _entryPointStdin = Nothing,
      _entryPointPackage = roots ^. rootsPackage,
      _entryPointPackageGlobal = roots ^. rootsPackageGlobal,
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
