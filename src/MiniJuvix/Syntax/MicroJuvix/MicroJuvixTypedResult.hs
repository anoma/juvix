module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
  ( module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module MiniJuvix.Syntax.MicroJuvix.InfoTable,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult (MicroJuvixArityResult)

data MicroJuvixTypedResult = MicroJuvixTypedResult
  { _resultMicroJuvixArityResult :: MicroJuvixArityResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MicroJuvixTypedResult

mainModule :: Lens' MicroJuvixTypedResult Module
mainModule = resultModules . _head
