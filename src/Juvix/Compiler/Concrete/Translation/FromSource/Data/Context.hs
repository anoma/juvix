module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState
import Juvix.Prelude

data ParserResult = ParserResult
  { _resultModule :: Module 'Parsed 'ModuleTop,
    _resultParserState :: ParserState
  }

makeLenses ''ParserResult
