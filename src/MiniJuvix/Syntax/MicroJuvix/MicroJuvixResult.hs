module MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult
  ( module MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult,
    module MiniJuvix.Syntax.MicroJuvix.InfoTable,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult qualified as Abstract
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language

data MicroJuvixResult = MicroJuvixResult
  { _resultAbstract :: Abstract.AbstractResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MicroJuvixResult
