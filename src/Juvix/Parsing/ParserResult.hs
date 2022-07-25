module Juvix.Parsing.ParserResult where

import Juvix.Parsing.InfoTable
import Juvix.Pipeline.EntryPoint
import Juvix.Prelude
import Juvix.Syntax.Concrete.Language

data ParserResult = ParserResult
  { _resultEntry :: EntryPoint,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty (Module 'Parsed 'ModuleTop)
  }
  deriving stock (Eq, Show)

makeLenses ''ParserResult
