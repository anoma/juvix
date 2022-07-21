module Juvix.Syntax.MicroJuvix.MicroJuvixResult
  ( module Juvix.Syntax.MicroJuvix.MicroJuvixResult,
    module Juvix.Syntax.MicroJuvix.InfoTable,
  )
where

import Juvix.Pipeline.EntryPoint qualified as E
import Juvix.Prelude
import Juvix.Syntax.Abstract.AbstractResult qualified as Abstract
import Juvix.Syntax.MicroJuvix.InfoTable
import Juvix.Syntax.MicroJuvix.Language

data MicroJuvixResult = MicroJuvixResult
  { _resultAbstract :: Abstract.AbstractResult,
    -- _resultTable :: InfoTable,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MicroJuvixResult

microJuvixResultEntryPoint :: Lens' MicroJuvixResult E.EntryPoint
microJuvixResultEntryPoint = resultAbstract . Abstract.abstractResultEntryPoint
