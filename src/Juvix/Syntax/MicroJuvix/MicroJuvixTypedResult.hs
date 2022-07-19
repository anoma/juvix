module Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult
  ( module Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module Juvix.Syntax.MicroJuvix.InfoTable,
  )
where

import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.InfoTable
import Juvix.Syntax.MicroJuvix.Language
import Juvix.Syntax.MicroJuvix.MicroJuvixArityResult (MicroJuvixArityResult)

type TypesTable = HashMap Name Expression

data MicroJuvixTypedResult = MicroJuvixTypedResult
  { _resultMicroJuvixArityResult :: MicroJuvixArityResult,
    _resultModules :: NonEmpty Module,
    _resultIdenTypes :: TypesTable
  }

makeLenses ''MicroJuvixTypedResult

mainModule :: Lens' MicroJuvixTypedResult Module
mainModule = resultModules . _head
