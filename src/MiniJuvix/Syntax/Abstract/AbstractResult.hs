module MiniJuvix.Syntax.Abstract.AbstractResult (
  module MiniJuvix.Syntax.Abstract.AbstractResult,
  module MiniJuvix.Syntax.Abstract.InfoTable,
  ) where

import MiniJuvix.Syntax.Abstract.InfoTable
import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Scoper.ScoperResult

data AbstractResult = AbstractResult {
  _resultScoper :: ScoperResult,
  _resultTable :: InfoTable,
  _resultModules :: NonEmpty TopModule
  }

makeLenses ''AbstractResult
