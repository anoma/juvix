module Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig where

import Juvix.Prelude.Base

data DependenciesConfig = DependenciesConfig
  { _dependenciesConfigForceUpdateLockfile :: Bool,
    _dependenciesConfigIgnorePackageNameConflicts :: Bool
  }

defaultDependenciesConfig :: DependenciesConfig
defaultDependenciesConfig =
  DependenciesConfig
    { _dependenciesConfigForceUpdateLockfile = False,
      _dependenciesConfigIgnorePackageNameConflicts = False
    }

makeLenses ''DependenciesConfig
