module MiniJuvix.Pipeline
  ( module MiniJuvix.Pipeline,
    module MiniJuvix.Pipeline.EntryPoint,
    module MiniJuvix.Pipeline.Stage,
  )
where

import Data.Kind qualified as GHC
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

runIOEither :: Sem '[Files, Error AJuvixError, Embed IO] a -> IO (Either AJuvixError a)
runIOEither = runM . runError . runFilesIO

runIO :: Sem '[Files, Error AJuvixError, Embed IO] a -> IO a
runIO = runIOEither >=> mayThrow
  where
    mayThrow :: Either AJuvixError r -> IO r
    mayThrow = \case
      Left err -> printErrorAnsi err >> exitFailure
      Right r -> return r

--------------------------------------------------------------------------------

upToParsing :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Parser.ParserResult
upToParsing = pipelineParser

upToScoping :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Scoper.ScoperResult
upToScoping = upToParsing >=> pipelineScoper

upToAbstract :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Abstract.AbstractResult
upToAbstract = upToScoping >=> pipelineAbstract

upToMicroJuvix :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r MicroJuvix.MicroJuvixResult
upToMicroJuvix = upToAbstract >=> pipelineMicroJuvix

upToMicroJuvixTyped :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r MicroJuvix.MicroJuvixTypedResult
upToMicroJuvixTyped = upToMicroJuvix >=> pipelineMicroJuvixTyped

upToMonoJuvix ::
  Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r MonoJuvix.MonoJuvixResult
upToMonoJuvix = upToMicroJuvixTyped >=> pipelineMonoJuvix

upToMiniHaskell ::
  Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r MiniHaskell.MiniHaskellResult
upToMiniHaskell = upToMonoJuvix >=> pipelineMiniHaskell

--------------------------------------------------------------------------------

pipelineParser :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Parser.ParserResult
pipelineParser = mapError (toAJuvixError @Text) . Parser.entryParser

pipelineScoper :: Members '[Files, Error AJuvixError] r => Parser.ParserResult -> Sem r Scoper.ScoperResult
pipelineScoper = mapError (toAJuvixError @Scoper.ScopeError) . Scoper.entryScoper

pipelineAbstract ::
  Members '[Files, Error AJuvixError] r =>
  Scoper.ScoperResult ->
  Sem r Abstract.AbstractResult
pipelineAbstract = mapError (toAJuvixError @Text) . Abstract.entryAbstract

pipelineMicroJuvix ::
  Members '[Files, Error AJuvixError] r =>
  Abstract.AbstractResult ->
  Sem r MicroJuvix.MicroJuvixResult
pipelineMicroJuvix = mapError (toAJuvixError @Text) . MicroJuvix.entryMicroJuvix

pipelineMicroJuvixTyped ::
  Members '[Files, Error AJuvixError] r =>
  MicroJuvix.MicroJuvixResult ->
  Sem r MicroJuvix.MicroJuvixTypedResult
pipelineMicroJuvixTyped = mapError (toAJuvixError @MicroJuvix.TypeCheckerError) . MicroJuvix.entryMicroJuvixTyped

pipelineMonoJuvix ::
  Members '[Files, Error AJuvixError] r =>
  MicroJuvix.MicroJuvixTypedResult ->
  Sem r MonoJuvix.MonoJuvixResult
pipelineMonoJuvix = mapError (toAJuvixError @Text) . MonoJuvix.entryMonoJuvix

pipelineMiniHaskell ::
  Members '[Files, Error AJuvixError] r =>
  MonoJuvix.MonoJuvixResult ->
  Sem r MiniHaskell.MiniHaskellResult
pipelineMiniHaskell = mapError (toAJuvixError @Text) . MiniHaskell.entryMiniHaskell
