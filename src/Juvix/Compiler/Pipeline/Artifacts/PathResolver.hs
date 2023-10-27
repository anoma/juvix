module Juvix.Compiler.Pipeline.Artifacts.PathResolver where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Package.Loader.PathResolver
import Juvix.Data.Effect.Git
import Juvix.Prelude

runPathResolverArtifacts :: (Members '[Files, Reader EntryPoint, State Artifacts, Error DependencyError, GitClone, Error JuvixError, EvalFileEff] r) => Sem (PathResolver ': r) a -> Sem r a
runPathResolverArtifacts = runStateLikeArtifacts runPathResolverPipe' artifactResolver

runPackagePathResolverArtifacts :: (Members '[Files, Reader EntryPoint, State Artifacts, Error DependencyError, GitClone, Error JuvixError, EvalFileEff] r) => Path Abs Dir -> Sem (PathResolver ': r) a -> Sem r a
runPackagePathResolverArtifacts root = runStateLikeArtifacts (runPackagePathResolver'' root) artifactResolver
