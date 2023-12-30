module Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig where

import Juvix.Prelude.Base

newtype DependenciesConfig = DependenciesConfig
  { _dependenciesConfigForceUpdateLockfile :: Bool
  }

defaultDependenciesConfig :: DependenciesConfig
defaultDependenciesConfig = DependenciesConfig {_dependenciesConfigForceUpdateLockfile = False}

makeLenses ''DependenciesConfig
