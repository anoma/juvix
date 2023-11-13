module Juvix.Compiler.Pipeline.Driver where

import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Setup (entrySetup)
import Juvix.Data.Effect.Git
import Juvix.Prelude

processFiles ::
  forall a b r.
  (Members '[Error JuvixError, Files, GitClone, PathResolver] r) =>
  (EntryPoint -> b -> Parser.ParserResult -> Sem r a) ->
  ([a] -> b) ->
  EntryPoint ->
  DependenciesConfig ->
  Sem r (NonEmpty a)
processFiles f coll entry depconfig = do
  entrySetup depconfig
  res <- Parser.fromSource entry
  -- TODO: process imports
  a <- f entry (coll []) res
  return $ nonEmpty' [a]
