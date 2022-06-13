module MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult
  ( module MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult (MicroJuvixResult)

data MicroJuvixArityResult = MicroJuvixArityResult
  { _resultMicroJuvixResult :: MicroJuvixResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MicroJuvixArityResult

mainModule :: Lens' MicroJuvixArityResult Module
mainModule = resultModules . _head
