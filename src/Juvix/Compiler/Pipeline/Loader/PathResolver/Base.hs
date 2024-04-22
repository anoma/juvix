module Juvix.Compiler.Pipeline.Loader.PathResolver.Base
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.Base,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig,
  )
where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
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
  -- | Given a relative file *with no extension*, returns the list of packages
  -- that contain that file. The file extension is also returned since it can be
  -- FileExtJuvix or FileExtJUvixMarkdown.
  ResolvePath :: Path Rel File -> PathResolver m [(PackageInfo, FileExt)]
  RegisterDependencies :: DependenciesConfig -> PathResolver m ()
  GetResolverState :: PathResolver m ResolverState
  ExpectedPathInfoTopModule :: TopModulePath -> PathResolver m PathInfoTopModule
  WithPath ::
    TopModulePath ->
    (Either PathResolverError (Path Abs Dir, Path Rel File) -> m x) ->
    PathResolver m x

makeLenses ''RootInfo
makeLenses ''PathInfoTopModule
makeSem ''PathResolver

withPathFile ::
  (Members '[PathResolver] r) =>
  TopModulePath ->
  (Either PathResolverError (Path Abs File) -> Sem r a) ->
  Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))
