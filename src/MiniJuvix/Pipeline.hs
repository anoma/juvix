module MiniJuvix.Pipeline (
  module MiniJuvix.Pipeline,
  module MiniJuvix.Pipeline.EntryPoint,
  ) where

import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Concrete.Parser as Parser
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as Scoper


pipelineParser :: Members '[Files, Error AJuvixError] r => EntryPoint -> Sem r Parser.ParserResult
pipelineParser = mapError (toAJuvixError @Text) . Parser.entryParser

pipelineScoper :: Members '[Files, Error AJuvixError] r => Parser.ParserResult -> Sem r Scoper.ScoperResult
pipelineScoper = mapError (toAJuvixError @Scoper.ScopeError) . Scoper.entryScope
