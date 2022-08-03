module Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Abstract.Data.NameDependencyInfo qualified as DepInfo
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context qualified as Abstract
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

data InternalResult = InternalResult
  { _resultAbstract :: Abstract.AbstractResult,
    -- _resultTable :: InfoTable,
    _resultModules :: NonEmpty Module,
    _resultDepInfo :: DepInfo.NameDependencyInfo
  }

makeLenses ''InternalResult

microJuvixResultEntryPoint :: Lens' InternalResult E.EntryPoint
microJuvixResultEntryPoint = resultAbstract . Abstract.abstractResultEntryPoint
