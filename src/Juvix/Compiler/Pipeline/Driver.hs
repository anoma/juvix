module Juvix.Compiler.Pipeline.Driver where

import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.Effect.Git
import Juvix.Prelude

processModule ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r Store.ModuleInfo
processModule entry = undefined

processDependencies ::
  forall r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  EntryPoint ->
  Sem r (Parser.ParserResult, Store.ModuleTable)
processDependencies entry = do
  res <- runReader entry upToParsing
  undefined
