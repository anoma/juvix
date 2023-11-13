module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Prelude

entrySetup ::
  (Member PathResolver r) =>
  DependenciesConfig ->
  Sem r ()
entrySetup = registerDependencies
