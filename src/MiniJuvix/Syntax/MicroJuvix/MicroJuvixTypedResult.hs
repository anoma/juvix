module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult (
  module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
  module MiniJuvix.Syntax.MicroJuvix.InfoTable,
  ) where


import MiniJuvix.Syntax.Abstract.AbstractResult qualified as Abstract
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.TypeChecker
import MiniJuvix.Prelude

data MicroJuvixTypedResult = MicroJuvixTypedResult {
  _resultAbstract :: Abstract.AbstractResult,
  _resultTable :: InfoTable,
  _resultModules :: NonEmpty Module,
  _resultLocalVars :: LocalVars
  }

makeLenses ''MicroJuvixTypedResult
