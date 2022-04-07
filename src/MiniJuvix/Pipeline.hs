module MiniJuvix.Pipeline (
  module MiniJuvix.Pipeline,
  module MiniJuvix.Pipeline.EntryPoint,
  module MiniJuvix.Pipeline.Stage,
  ) where

import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Concrete.Parser as Parser
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as Scoper
import MiniJuvix.Pipeline.Stage
import qualified Data.Kind as GHC
import GHC.IO.Exception
import qualified MiniJuvix.Syntax.Abstract.AbstractResult as Abstract
import qualified MiniJuvix.Translation.ScopedToAbstract as Abstract


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


runIO :: Sem '[Files, Error AJuvixError, Embed IO] a -> IO a
runIO = (runM . runError . runFilesIO) >=> mayThrow
  where
  mayThrow :: Either AJuvixError r -> IO r
  mayThrow = \case
    Left r -> printErrorAnsi r >> ioError (userError "")
    Right r -> return r

upToParsing :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Parser.ParserResult
upToParsing = pipelineParser

upToScoping :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Scoper.ScoperResult
upToScoping = upToParsing >=> pipelineScoper

upToAbstract :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Abstract.AbstractResult
upToAbstract = upToScoping >=> pipelineAbstract

upToMicroJuvix :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Abstract.AbstractResult
upToMicroJuvix = upToScoping >=> pipelineAbstract

upToMicroJuvixTyped :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Abstract.AbstractResult
upToMicroJuvixTyped = upToScoping >=> pipelineAbstract


pipelineParser :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Parser.ParserResult
pipelineParser = mapError (toAJuvixError @Text) . Parser.entryParser

pipelineScoper :: Members '[Files, Error AJuvixError] r => Parser.ParserResult -> Sem r Scoper.ScoperResult
pipelineScoper = mapError (toAJuvixError @Scoper.ScopeError) . Scoper.entryScope

pipelineAbstract :: Members '[Files, Error AJuvixError] r => Scoper.ScoperResult
   -> Sem r Abstract.AbstractResult
pipelineAbstract = mapError (toAJuvixError @Text) . Abstract.entryAbstract
