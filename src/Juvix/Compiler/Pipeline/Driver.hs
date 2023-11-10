module Juvix.Compiler.Pipeline.Driver where

import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Prelude

processFiles ::
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  (EntryPoint -> ParsedResult -> a) ->
  EntryPoint ->
  DependenciesConfig ->
  Sem r a
processFiles entry depconfig = undefined
