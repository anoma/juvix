module MiniJuvix.Syntax.Concrete.Scoped.Scoper.ScoperResult where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as Scoped

data ScoperResult = ScoperResult
  { _resultParserResult :: Parser.ParserResult,
    _resultParserTable :: Parser.InfoTable,
    _resultScoperTable :: Scoped.InfoTable,
    _resultModules :: NonEmpty (Module 'Scoped 'ModuleTop)
  }
  deriving stock (Eq, Show)

makeLenses ''ScoperResult
