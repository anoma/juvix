module MiniJuvix.Syntax.Concrete.Parser.ParserResult where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Parser.InfoTable
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Pipeline.EntryPoint

data ParserResult = ParserResult {
  _resultEntry :: EntryPoint,
  _resultTable :: InfoTable,
  _resultModules :: NonEmpty (Module 'Parsed 'ModuleTop)
  }

makeLenses ''ParserResult
