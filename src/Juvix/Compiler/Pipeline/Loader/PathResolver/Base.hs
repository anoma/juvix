module Juvix.Compiler.Pipeline.Loader.PathResolver.Base
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.Base,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig,
  )
where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.ImportNode
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Loader.PathResolver.Paths
import Juvix.Prelude

data RootKind
  = RootKindPackage
  | RootKindSingleFile
  deriving stock (Show)

data RootInfo = RootInfo
  { _rootInfoPath :: Path Abs Dir,
    _rootInfoKind :: RootKind
  }
  deriving stock (Show)

data PathInfoTopModule = PathInfoTopModule
  { _pathInfoTopModule :: TopModulePath,
    _pathInfoRootInfo :: RootInfo
  }
  deriving stock (Show)

data PathResolver :: Effect where
  RegisterDependencies :: DependenciesConfig -> PathResolver m ()
  GetPackageInfos :: PathResolver m (HashMap (Path Abs Dir) PackageInfo)
  ExpectedPathInfoTopModule :: TopModulePath -> PathResolver m PathInfoTopModule
  -- | Given a relative file *with no extension*, returns the list of packages
  -- that contain that file. The file extension is also returned since it can be
  -- FileExtJuvix or FileExtJuvixMarkdown.
  ResolvePath :: ImportScan -> PathResolver m (PackageInfo, FileExt)
  -- | The root is assumed to be a package root.
  WithResolverRoot :: Path Abs Dir -> m a -> PathResolver m a
  SupportsParallel :: PathResolver m Bool
  ResolverRoot :: PathResolver m (Path Abs Dir)
  ResolverInitialRoot :: PathResolver m (Path Abs Dir)

makeLenses ''RootInfo
makeLenses ''PathInfoTopModule
makeSem ''PathResolver

-- getCurrentPackageInfo :: (Members '[PathResolver] r) => Sem r PackageInfo
-- getCurrentPackageInfo = do
--   tbl <- getPackageInfos
--   r <- resolverRoot
--   let err =
--         impossibleError
--           ( "The current root has not been registered as a package.\nCurrent root = "
--               <> show r
--           )
--   return (fromMaybe err (tbl ^. at r))

withPathFile ::
  (Members '[PathResolver] r) =>
  TopModulePath ->
  (ImportNode -> Sem r a) ->
  Sem r a
withPathFile m f = do
  node <- resolveTopModulePath m
  let root = node ^. importNodePackageRoot
  withResolverRoot root (f node)

-- | Returns the root of the package where the module belongs and the path to
-- the module relative to the root.
resolveTopModulePath ::
  (Members '[PathResolver] r) =>
  TopModulePath ->
  Sem r ImportNode
resolveTopModulePath mp = do
  let scan = topModulePathToImportScan mp
      relpath = topModulePathToRelativePathNoExt mp
  (pkg, ext) <- resolvePath scan
  let node =
        ImportNode
          { _importNodeFile = addFileExt ext relpath,
            _importNodePackageRoot = pkg ^. packageRoot
          }
  return node
