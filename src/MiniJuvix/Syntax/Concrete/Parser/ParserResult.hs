module MiniJuvix.Syntax.Concrete.Parser.ParserResult where

import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Parser.InfoTable

data ParserResult = ParserResult
  { _resultEntry :: EntryPoint,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty (Module 'Parsed 'ModuleTop)
  }
  deriving stock (Eq, Show)

makeLenses ''ParserResult
