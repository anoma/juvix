module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig,
  )
where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Prelude

data Root = 
       RootGlobalStdlib 
     | RootGlobalPackage
     | RootLocalPackage
      deriving stock (Show, Eq, Ord) 

data PackageRoot = PackageRoot
  { _packageRoot :: Path Abs Dir,
    _packageRootType :: Root
  }
  deriving stock (Show, Eq, Ord)

data PathInfoTopModule = PathInfoTopModule
  { _pathInfoTopModule :: TopModulePath,
    _pathInfoPackageRoot :: Maybe PackageRoot,
    _pathInfoRelPath :: Path Rel File,
    _pathInfoFileExt :: FileExt
  }
data PathResolver m a where
  RegisterDependencies :: DependenciesConfig -> PathResolver m ()
  ExpectedModulePath :: Path Abs File -> TopModulePath 
    -> PathResolver m (Maybe PathInfoTopModule)
  WithPath ::
    TopModulePath ->
    (Either PathResolverError (Path Abs Dir, Path Rel File) -> m x) ->
    PathResolver m x

makeLenses ''PackageRoot
makeLenses ''PathInfoTopModule
makeSem ''PathResolver

withPathFile :: (Members '[PathResolver] r) => TopModulePath -> (Either PathResolverError (Path Abs File) -> Sem r a) -> Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))

pathInfoAbsPath :: Lens' PathInfoTopModule (Path Abs File)
pathInfoAbsPath = impossible
--  pathInfoPackageRoot . packageRoot . to (<//>) . pathInfoRelPath 