module Juvix.Compiler.Pipeline
  ( module Juvix.Compiler.Pipeline,
    module Juvix.Compiler.Pipeline.EntryPoint,
  )
where

import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Setup
import Juvix.Prelude

type PipelineEff = '[Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

--------------------------------------------------------------------------------
-- Workflows
--------------------------------------------------------------------------------

typecheck ::
  Members PipelineEff r =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
typecheck =
  do
    Concrete.fromSource
    >=> Abstract.fromConcrete
    >=> Internal.fromAbstract
    >=> Internal.arityChecking
    >=> Internal.typeChecking

compile ::
  Members PipelineEff r =>
  EntryPoint ->
  Sem r C.MiniCResult
compile = typecheck >=> C.fromInternal

--------------------------------------------------------------------------------

upToParsing ::
  Members '[Files, Error JuvixError, NameIdGen] r =>
  EntryPoint ->
  Sem r Parser.ParserResult
upToParsing = entrySetup >=> Parser.fromSource

upToScoping ::
  Members '[Files, NameIdGen, Error JuvixError] r =>
  EntryPoint ->
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >=> Scoper.fromParsed

upToAbstract ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >=> Abstract.fromConcrete

upToInternal ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalResult
upToInternal = upToAbstract >=> Internal.fromAbstract

upToInternalArity ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalArityResult
upToInternalArity = upToInternal >=> Internal.arityChecking

upToInternalTyped ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
upToInternalTyped = upToInternalArity >=> Internal.typeChecking

upToInternalReachability ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Internal.InternalTypedResult
upToInternalReachability =
  upToInternalTyped
    >=> return . Internal.filterUnreachable

upToMiniC ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r C.MiniCResult
upToMiniC = upToInternalReachability >=> C.fromInternal

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
