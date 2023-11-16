module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig,
  )
where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Paths
import Juvix.Prelude

data RootKind
  = RootKindGlobalStdlib
  | RootKindGlobalPackage
  | RootKindLocalPackage
  deriving stock (Show, Eq, Ord)

data RootInfo = RootInfo
  { _rootInfoPath :: Path Abs Dir,
    _rootInfoKind :: RootKind
  }
  deriving stock (Show, Eq, Ord)

data PathInfoTopModule = PathInfoTopModule
  { _pathInfoTopModule :: TopModulePath,
    _pathInfoRootInfo :: Maybe RootInfo
  }

data PathResolver m a where
  RegisterDependencies :: DependenciesConfig -> PathResolver m ()
  ExpectedPathInfoTopModule :: TopModulePath -> PathResolver m PathInfoTopModule
  WithPath ::
    TopModulePath ->
    (Either PathResolverError (Path Abs Dir, Path Rel File) -> m x) ->
    PathResolver m x

makeLenses ''RootInfo
makeLenses ''PathInfoTopModule
makeSem ''PathResolver

withPathFile :: (Members '[PathResolver] r) => TopModulePath -> (Either PathResolverError (Path Abs File) -> Sem r a) -> Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))

pathInfoPath :: Lens' PathInfoTopModule (Path Rel File)
pathInfoPath = pathInfoTopModule . topModulePathToRelativePath'
