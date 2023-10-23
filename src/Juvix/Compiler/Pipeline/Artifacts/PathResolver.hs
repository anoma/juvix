module Juvix.Compiler.Pipeline.Artifacts.PathResolver where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.Git
import Juvix.Prelude

runPathResolverArtifacts :: (Members '[Files, Reader EntryPoint, State Artifacts, Error DependencyError, GitClone] r) => Sem (PathResolver ': r) a -> Sem r a
runPathResolverArtifacts = runStateLikeArtifacts runPathResolverPipe' artifactResolver
