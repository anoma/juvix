module MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult
  ( module MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult,
    module MiniJuvix.Syntax.MonoJuvix.InfoTable,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as Micro
import MiniJuvix.Syntax.MonoJuvix.InfoTable
import MiniJuvix.Syntax.MonoJuvix.Language

data MonoJuvixResult = MonoJuvixResult
  { _resultMicroTyped :: Micro.MicroJuvixTypedResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MonoJuvixResult
