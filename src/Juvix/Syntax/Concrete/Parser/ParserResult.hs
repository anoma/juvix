module Juvix.Syntax.Concrete.Parser.ParserResult where

import Juvix.Pipeline.EntryPoint
import Juvix.Prelude
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Parser.InfoTable

data ParserResult = ParserResult
  { _resultEntry :: EntryPoint,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty (Module 'Parsed 'ModuleTop)
  }
  deriving stock (Eq, Show)

makeLenses ''ParserResult
