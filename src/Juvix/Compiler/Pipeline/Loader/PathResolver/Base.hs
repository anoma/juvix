module Juvix.Compiler.Pipeline.Loader.PathResolver.Base
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.Base,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig,
  )
where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Prelude

data PathResolver m a where
  RegisterDependencies :: DependenciesConfig -> PathResolver m ()
  ExpectedModulePath :: Path Abs File -> TopModulePath -> PathResolver m (Maybe (Path Abs File))
  WithPath ::
    TopModulePath ->
    (Either PathResolverError (Path Abs Dir, Path Rel File) -> m x) ->
    PathResolver m x

makeSem ''PathResolver

withPathFile :: (Members '[PathResolver] r) => TopModulePath -> (Either PathResolverError (Path Abs File) -> Sem r a) -> Sem r a
withPathFile m f = withPath m (f . mapRight (uncurry (<//>)))
