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
import Juvix.Compiler.Core.Translation qualified as Core
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Setup
import Juvix.Prelude

type PipelineEff = '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

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

upToParsing ::
  Members '[Reader EntryPoint, Files, Error JuvixError, NameIdGen] r =>
  Sem r Parser.ParserResult
upToParsing = entrySetup >>= Parser.fromSource

upToScoping ::
  Members '[Reader EntryPoint, Files, NameIdGen, Error JuvixError] r =>
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >>= Scoper.fromParsed

upToAbstract ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >>= Abstract.fromConcrete

upToInternal ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Internal.InternalResult
upToInternal = upToAbstract >>= Internal.fromAbstract

upToInternalArity ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Internal.InternalArityResult
upToInternalArity = upToInternal >>= Internal.arityChecking

upToInternalTyped ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Internal.InternalTypedResult
upToInternalTyped = upToInternalArity >>= Internal.typeChecking

upToInternalReachability ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Internal.InternalTypedResult
upToInternalReachability =
  Internal.filterUnreachable <$> upToInternalTyped

upToCore ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r Core.CoreResult
upToCore =
  upToInternalReachability >>= Core.fromInternal

upToMiniC ::
  Members '[Reader EntryPoint, Files, NameIdGen, Builtins, Error JuvixError] r =>
  Sem r C.MiniCResult
upToMiniC = upToInternalReachability >>= C.fromInternal

runIOEither :: EntryPoint -> Sem PipelineEff a -> IO (Either JuvixError a)
runIOEither entry =
  runM
    . runError
    . runBuiltins
    . runNameIdGen
    . mapError (JuvixError @FilesError)
    . runFilesIO (entry ^. entryPointRoot)
    . runReader entry

runIO :: GenericOptions -> EntryPoint -> Sem PipelineEff a -> IO a
runIO opts entry = runIOEither entry >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> runM $ runReader opts $ printErrorAnsiSafe err >> embed exitFailure
      Right r -> return r

runIO' :: EntryPoint -> Sem PipelineEff a -> IO a
runIO' = runIO defaultGenericOptions
