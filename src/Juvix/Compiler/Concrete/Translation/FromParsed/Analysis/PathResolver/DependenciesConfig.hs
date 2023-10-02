module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.DependenciesConfig where

import Juvix.Prelude.Base

newtype DependenciesConfig = DependenciesConfig
  { _dependenciesConfigForceUpdateLockfile :: Bool
  }

defaultDependenciesConfig :: DependenciesConfig
defaultDependenciesConfig = DependenciesConfig {_dependenciesConfigForceUpdateLockfile = True}

makeLenses ''DependenciesConfig
