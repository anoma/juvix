module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
  ( module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module MiniJuvix.Syntax.MicroJuvix.InfoTable,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult

data MicroJuvixTypedResult = MicroJuvixTypedResult
  { _resultMicroJuvixResult :: MicroJuvixResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MicroJuvixTypedResult
