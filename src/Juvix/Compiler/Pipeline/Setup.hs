module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.Git
import Juvix.Prelude

entrySetup ::
  (Members '[Reader EntryPoint, Files, GitClone, PathResolver] r) =>
  DependenciesConfig ->
  Sem r ()
entrySetup = registerDependencies
