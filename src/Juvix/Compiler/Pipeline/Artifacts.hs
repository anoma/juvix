-- | Stuff that is generated when the pipeline is run
module Juvix.Compiler.Pipeline.Artifacts where

import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

data Artifacts = Artifacts
  { _artifactBuiltins :: BuiltinsState,
    _artifactResolver :: ResolverState,
    _artifactNameIdState :: Stream NameId,
    _artifactTypes :: TypesTable,
    _artifactFunctions :: FunctionsTable
  }

makeLenses ''Artifacts

runPathResolverArtifacts :: Members '[Files, Reader EntryPoint, State Artifacts] r => Sem (PathResolver ': r) a -> Sem r a
runPathResolverArtifacts m = do
  (s', a) <- runPathResolverPipe m
  modify' (set artifactResolver s')
  return a

runBuiltinsArtifacts :: Members '[Error JuvixError, State Artifacts] r => Sem (Builtins ': r) a -> Sem r a
runBuiltinsArtifacts m = do
  s <- gets (^. artifactBuiltins)
  (s', a) <- runBuiltins s m
  modify' (set artifactBuiltins s')
  return a

runNameIdGenArtifacts ::
  Members '[State Artifacts] r =>
  Sem (NameIdGen ': r) a ->
  Sem r a
runNameIdGenArtifacts m = do
  s <- gets (^. artifactNameIdState)
  (s', a) <- runNameIdGen s m
  modify' (set artifactNameIdState s')
  return a
