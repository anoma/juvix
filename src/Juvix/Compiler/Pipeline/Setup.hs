module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

entrySetup ::
  (Members '[Reader EntryPoint, Files, Git, PathResolver] r) =>
  Sem r ()
entrySetup = registerDependencies
