module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig,
  )
where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Prelude

data RootKind
  = RootKindGlobalPackage
  | RootKindLocalPackage
  deriving stock (Show)

data RootInfo = RootInfo
  { _rootInfoPath :: Path Abs Dir,
    _rootInfoKind :: RootKind
  }
  deriving stock (Show)

data PathInfoTopModule = PathInfoTopModule
  { _pathInfoTopModule :: TopModulePath,
    _pathInfoRootInfo :: Maybe RootInfo
  }
  deriving stock (Show)

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
