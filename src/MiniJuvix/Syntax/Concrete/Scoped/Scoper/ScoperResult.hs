module MiniJuvix.Syntax.Concrete.Scoped.Scoper.ScoperResult where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.InfoTable as Scoped
import qualified MiniJuvix.Syntax.Concrete.Parser.ParserResult as Parser


data ScoperResult = ScoperResult {
  _resultParserResult :: Parser.ParserResult,
  _resultScoperTable :: Scoped.InfoTable,
  _resultModules :: NonEmpty (Module 'Scoped 'ModuleTop)
  }
makeLenses ''ScoperResult
