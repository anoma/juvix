module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
  ( module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context,
    module Juvix.Compiler.Concrete.Data.ParsedInfoTable,
  )
where

import Juvix.Compiler.Concrete.Data.ParsedInfoTable
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

data ParserResult = ParserResult
  { _resultEntry :: EntryPoint,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty (Module 'Parsed 'ModuleTop)
  }
  deriving stock (Eq, Show)

makeLenses ''ParserResult
