module Juvix.Syntax.MicroJuvix.MicroJuvixArityResult
  ( module Juvix.Syntax.MicroJuvix.MicroJuvixArityResult,
  )
where

import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language
import Juvix.Syntax.MicroJuvix.MicroJuvixResult (MicroJuvixResult)

data MicroJuvixArityResult = MicroJuvixArityResult
  { _resultMicroJuvixResult :: MicroJuvixResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MicroJuvixArityResult

mainModule :: Lens' MicroJuvixArityResult Module
mainModule = resultModules . _head
