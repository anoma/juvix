module MiniJuvix.Pipeline
  ( module MiniJuvix.Pipeline,
    module MiniJuvix.Pipeline.EntryPoint,
    module MiniJuvix.Pipeline.Stage,
  )
where

import Data.Kind qualified as GHC
import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Pipeline.Stage
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult qualified as Abstract
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult qualified as MicroJuvix
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as MicroJuvix
import MiniJuvix.Syntax.MicroJuvix.TypeChecker qualified as MicroJuvix
import MiniJuvix.Translation.AbstractToMicroJuvix qualified as MicroJuvix
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as MonoJuvix
import MiniJuvix.Translation.MonoJuvixToMiniC qualified as MiniC
import MiniJuvix.Translation.MonoJuvixToMiniHaskell qualified as MiniHaskell
import MiniJuvix.Translation.ScopedToAbstract qualified as Abstract

type StageInput :: Pipe -> GHC.Type
type family StageInput c = res where
  StageInput 'Entry = EntryPoint
  StageInput 'Parsing = EntryPoint
  StageInput 'Scoping = Parser.ParserResult
  StageInput 'Abstract = Scoper.ScoperResult

type StageResult :: Pipe -> GHC.Type
type family StageResult c = res | res -> c where
  StageResult 'Entry = EntryPoint
  StageResult 'Parsing = Parser.ParserResult
  StageResult 'Scoping = Scoper.ScoperResult
  StageResult 'Abstract = Abstract.AbstractResult

--------------------------------------------------------------------------------

type PipelineEff = '[Files, NameIdGen, Error MiniJuvixError, Embed IO]

runIOEither :: Sem PipelineEff a -> IO (Either MiniJuvixError a)
runIOEither = runM . runError . runNameIdGen . runFilesIO

runIO :: Sem PipelineEff a -> IO a
runIO = runIOEither >=> mayThrow
  where
    mayThrow :: Either MiniJuvixError r -> IO r
    mayThrow = \case
      Left err -> printErrorAnsiSafe err >> exitFailure
      Right r -> return r

--------------------------------------------------------------------------------

upToParsing ::
  Members '[Files, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r Parser.ParserResult
upToParsing = pipelineParser

upToScoping ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r Scoper.ScoperResult
upToScoping = upToParsing >=> pipelineScoper

upToAbstract ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r Abstract.AbstractResult
upToAbstract = upToScoping >=> pipelineAbstract

upToMicroJuvix ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r MicroJuvix.MicroJuvixResult
upToMicroJuvix = upToAbstract >=> pipelineMicroJuvix

upToMicroJuvixTyped ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r MicroJuvix.MicroJuvixTypedResult
upToMicroJuvixTyped = upToMicroJuvix >=> pipelineMicroJuvixTyped

upToMonoJuvix ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r MonoJuvix.MonoJuvixResult
upToMonoJuvix = upToMicroJuvixTyped >=> pipelineMonoJuvix

upToMiniHaskell ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r MiniHaskell.MiniHaskellResult
upToMiniHaskell = upToMonoJuvix >=> pipelineMiniHaskell

upToMiniC ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r MiniC.MiniCResult
upToMiniC = upToMonoJuvix >=> pipelineMiniC

--------------------------------------------------------------------------------

pipelineParser ::
  Members '[Files, Error MiniJuvixError] r =>
  EntryPoint ->
  Sem r Parser.ParserResult
pipelineParser = mapError (MiniJuvixError @Parser.ParserError) . Parser.entryParser

pipelineScoper ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  Parser.ParserResult ->
  Sem r Scoper.ScoperResult
pipelineScoper = mapError (MiniJuvixError @Scoper.ScoperError) . Scoper.entryScoper

pipelineAbstract ::
  Scoper.ScoperResult ->
  Sem r Abstract.AbstractResult
pipelineAbstract = Abstract.entryAbstract

pipelineMicroJuvix ::
  Abstract.AbstractResult ->
  Sem r MicroJuvix.MicroJuvixResult
pipelineMicroJuvix = MicroJuvix.entryMicroJuvix

pipelineMicroJuvixTyped ::
  Members '[Files, NameIdGen, Error MiniJuvixError] r =>
  MicroJuvix.MicroJuvixResult ->
  Sem r MicroJuvix.MicroJuvixTypedResult
pipelineMicroJuvixTyped =
  mapError (MiniJuvixError @MicroJuvix.TypeCheckerError) . MicroJuvix.entryMicroJuvixTyped

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
  MonoJuvix.MonoJuvixResult ->
  Sem r MiniC.MiniCResult
pipelineMiniC = MiniC.entryMiniC
