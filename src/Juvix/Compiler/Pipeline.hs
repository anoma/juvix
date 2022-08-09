module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
  )
where

-- import Juvix.Compiler.Abstract.Translation.FromConcrete qualified as Abstract

import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.C.Translation.FromInternal qualified as MiniC
import Juvix.Compiler.Backend.Haskell qualified as Haskell
import Juvix.Compiler.Backend.Haskell.Translation.FromMono qualified as MiniHaskell
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete qualified as Concrete
--------------------------------------------------------------------------------

import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Internal.Translation.FromAbstract qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal qualified as FromInternal
import Juvix.Compiler.Internal.Translation.FromInternal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Checker qualified as Internal
  ( TypeCheckerError,
  )
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
  ( InternalTypedResult,
  )
import Juvix.Compiler.Mono qualified as Mono
import Juvix.Compiler.Mono.Translation.FromInternal qualified as MonoJuvix
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Setup qualified as Setup
import Juvix.Prelude

type PipelineEff = '[Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

typechecking ::
  Members PipelineEff r =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
typechecking =
  do
    Concrete.fromSource
    >=> Abstract.fromConcrete
    >=> Internal.fromAbstract
    >=> Internal.arityChecking
    >=> Internal.typeChecking

toC ::
  Members PipelineEff r =>
  EntryPoint ->
  Sem r C.MiniCResult
toC = typechecking >=> C.fromInternal

toHaskell ::
  Members PipelineEff r =>
  EntryPoint ->
  Sem r Haskell.Context
toHaskell = do
  typechecking
    >=> Mono.fromInternal
    >=> Haskell.fromMono

--------------------------------------------------------------------------------

runIOEither :: Sem PipelineEff a -> IO (Either JuvixError a)
runIOEither = runM . runError . runBuiltins . runNameIdGen . mapError (JuvixError @FilesError) . runFilesIO

runIO :: Sem PipelineEff a -> IO a
runIO = runIOEither >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> printErrorAnsiSafe err >> exitFailure
      Right r -> return r

upToSetup ::
  Member Files r =>
  EntryPoint ->
  Sem r EntryPoint
upToSetup = Setup.entrySetup

upToParsing ::
  Members '[Files, Error JuvixError, NameIdGen] r =>
  EntryPoint ->
  Sem r Parser.ParserResult
upToParsing = upToSetup >=> Parser.fromSource

upToScoping ::
  Members '[Files, NameIdGen, Error JuvixError] r =>
  EntryPoint ->
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >=> Scoper.fromParsed

upToAbstract ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >=> pipelineAbstract

upToInternal ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalResult
upToInternal = upToAbstract >=> Internal.fromAbstract

upToInternalArity ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalArityResult
upToInternalArity = upToInternal >=> FromInternal.arityChecking

upToInternalTyped ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
upToInternalTyped = upToInternalArity >=> pipelineInternalTyped

upToInternalReachability ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
upToInternalReachability = upToInternalTyped >=> pipelineInternalReachability

upToMonoJuvix ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MonoJuvix.MonoJuvixResult
upToMonoJuvix = upToInternalTyped >=> pipelineMonoJuvix

upToMiniHaskell ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MiniHaskell.Context
upToMiniHaskell = upToMonoJuvix >=> pipelineMiniHaskell

upToMiniC ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MiniC.MiniCResult
upToMiniC = upToInternalReachability >=> pipelineMiniC

pipelineAbstract ::
  Members '[Error JuvixError, Builtins, NameIdGen] r =>
  Scoper.ScoperResult ->
  Sem r Abstract.AbstractResult
pipelineAbstract = mapError (JuvixError @Scoper.ScoperError) . Abstract.fromConcrete

pipelineInternalTyped ::
  Members '[Files, NameIdGen, Error JuvixError] r =>
  Internal.InternalArityResult ->
  Sem r Internal.InternalTypedResult
pipelineInternalTyped =
  mapError (JuvixError @Internal.TypeCheckerError) . FromInternal.typeChecking

pipelineInternalReachability :: Internal.InternalTypedResult -> Sem r Internal.InternalTypedResult
pipelineInternalReachability = return . FromInternal.filterUnreachable

pipelineMonoJuvix ::
  Members '[Files, NameIdGen] r =>
  Internal.InternalTypedResult ->
  Sem r MonoJuvix.MonoJuvixResult
pipelineMonoJuvix = MonoJuvix.fromInternal

pipelineMiniHaskell ::
  MonoJuvix.MonoJuvixResult ->
  Sem r MiniHaskell.Context
pipelineMiniHaskell = MiniHaskell.fromMono

pipelineMiniC ::
  Member Builtins r =>
  Internal.InternalTypedResult ->
  Sem r MiniC.MiniCResult
pipelineMiniC = MiniC.fromInternal
