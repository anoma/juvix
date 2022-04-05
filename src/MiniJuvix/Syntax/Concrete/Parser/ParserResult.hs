module MiniJuvix.Syntax.Concrete.Parser.ParserResult where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Parser.InfoTable
import MiniJuvix.Syntax.Concrete.Language

data ParserResult = ParserResult {
  _resultTable :: InfoTable,
  _resultModules :: Module 'Parsed 'ModuleTop
  }

makeLenses ''ParserResult
