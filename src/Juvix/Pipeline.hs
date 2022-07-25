module Juvix.Pipeline
  ( module Juvix.Pipeline,
    module Juvix.Pipeline.EntryPoint,
  )
where

import Juvix.Analysis.Arity qualified as MicroJuvix
import Juvix.Analysis.Scoping qualified as Scoper
import Juvix.Analysis.TypeChecking qualified as MicroJuvix
import Juvix.Builtins
import Juvix.Internal.NameIdGen
import Juvix.Parsing.Parser qualified as Parser
import Juvix.Pipeline.EntryPoint
import Juvix.Pipeline.Setup qualified as Setup
import Juvix.Prelude
import Juvix.Syntax.Abstract.AbstractResult qualified as Abstract
import Juvix.Syntax.MicroJuvix.MicroJuvixResult qualified as MicroJuvix
import Juvix.Translation.AbstractToMicroJuvix qualified as MicroJuvix
import Juvix.Translation.MicroJuvixToMiniC qualified as MiniC
import Juvix.Translation.MicroJuvixToMonoJuvix qualified as MonoJuvix
import Juvix.Translation.MonoJuvixToMiniHaskell qualified as MiniHaskell
import Juvix.Translation.ScopedToAbstract qualified as Abstract

type PipelineEff = '[Files, NameIdGen, Builtins, Error JuvixError, Embed IO]

runIOEither :: Sem PipelineEff a -> IO (Either JuvixError a)
runIOEither = runM . runError . runBuiltins . runNameIdGen . mapError (JuvixError @FilesError) . runFilesIO

runIO :: Sem PipelineEff a -> IO a
runIO = runIOEither >=> mayThrow
  where
    mayThrow :: Either JuvixError r -> IO r
    mayThrow = \case
      Left err -> printErrorAnsiSafe err >> exitFailure
      Right r -> return r

--------------------------------------------------------------------------------

upToSetup ::
  Member Files r =>
  EntryPoint ->
  Sem r EntryPoint
upToSetup = Setup.entrySetup

upToParsing ::
  Members '[Files, Error JuvixError] r =>
  EntryPoint ->
  Sem r Parser.ParserResult
upToParsing = upToSetup >=> pipelineParser

upToScoping ::
  Members '[Files, NameIdGen, Error JuvixError] r =>
  EntryPoint ->
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >=> pipelineScoper

upToAbstract ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >=> pipelineAbstract

upToMicroJuvix ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MicroJuvix.MicroJuvixResult
upToMicroJuvix = upToAbstract >=> pipelineMicroJuvix

upToMicroJuvixArity ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MicroJuvix.MicroJuvixArityResult
upToMicroJuvixArity = upToMicroJuvix >=> pipelineMicroJuvixArity

upToMicroJuvixTyped ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MicroJuvix.MicroJuvixTypedResult
upToMicroJuvixTyped = upToMicroJuvixArity >=> pipelineMicroJuvixTyped

upToMonoJuvix ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MonoJuvix.MonoJuvixResult
upToMonoJuvix = upToMicroJuvixTyped >=> pipelineMonoJuvix

upToMiniHaskell ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MiniHaskell.MiniHaskellResult
upToMiniHaskell = upToMonoJuvix >=> pipelineMiniHaskell

upToMiniC ::
  Members '[Files, NameIdGen, Builtins, Error JuvixError] r =>
  EntryPoint ->
  Sem r MiniC.MiniCResult
upToMiniC = upToMicroJuvixTyped >=> pipelineMiniC

--------------------------------------------------------------------------------

pipelineParser ::
  Members '[Files, Error JuvixError] r =>
  EntryPoint ->
  Sem r Parser.ParserResult
pipelineParser = mapError (JuvixError @Parser.ParserError) . Parser.entryParser

pipelineScoper ::
  Members '[Files, NameIdGen, Error JuvixError] r =>
  Parser.ParserResult ->
  Sem r Scoper.ScoperResult
pipelineScoper = mapError (JuvixError @Scoper.ScoperError) . Scoper.entryScoper

pipelineAbstract ::
  Members '[Error JuvixError, Builtins, NameIdGen] r =>
  Scoper.ScoperResult ->
  Sem r Abstract.AbstractResult
pipelineAbstract = mapError (JuvixError @Scoper.ScoperError) . Abstract.entryAbstract

pipelineMicroJuvix ::
  Members '[Error JuvixError] r =>
  Abstract.AbstractResult ->
  Sem r MicroJuvix.MicroJuvixResult
pipelineMicroJuvix = MicroJuvix.entryMicroJuvix

pipelineMicroJuvixArity ::
  Members '[Error JuvixError, NameIdGen] r =>
  MicroJuvix.MicroJuvixResult ->
  Sem r MicroJuvix.MicroJuvixArityResult
pipelineMicroJuvixArity = mapError (JuvixError @MicroJuvix.ArityCheckerError) . MicroJuvix.entryMicroJuvixArity

pipelineMicroJuvixTyped ::
  Members '[Files, NameIdGen, Error JuvixError] r =>
  MicroJuvix.MicroJuvixArityResult ->
  Sem r MicroJuvix.MicroJuvixTypedResult
pipelineMicroJuvixTyped =
  mapError (JuvixError @MicroJuvix.TypeCheckerError) . MicroJuvix.entryMicroJuvixTyped

pipelineMonoJuvix ::
  Members '[Files, NameIdGen] r =>
  MicroJuvix.MicroJuvixTypedResult ->
  Sem r MonoJuvix.MonoJuvixResult
pipelineMonoJuvix = MonoJuvix.entryMonoJuvix

pipelineMiniHaskell ::
  MonoJuvix.MonoJuvixResult ->
  Sem r MiniHaskell.MiniHaskellResult
pipelineMiniHaskell = MiniHaskell.entryMiniHaskell

pipelineMiniC ::
  Member Builtins r =>
  MicroJuvix.MicroJuvixTypedResult ->
  Sem r MiniC.MiniCResult
pipelineMiniC = MiniC.entryMiniC
