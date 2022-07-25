module Juvix.Analysis.Scoping.ScoperResult where

import Juvix.Parsing.Parser qualified as Parser
import Juvix.Prelude
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Scoped.InfoTable qualified as Scoped

data ScoperResult = ScoperResult
  { _resultParserResult :: Parser.ParserResult,
    _resultParserTable :: Parser.InfoTable,
    _resultScoperTable :: Scoped.InfoTable,
    _resultModules :: NonEmpty (Module 'Scoped 'ModuleTop)
  }

makeLenses ''ScoperResult
