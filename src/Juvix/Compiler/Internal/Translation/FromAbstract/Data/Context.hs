module Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Abstract.Data.NameDependencyInfo qualified as DepInfo
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context qualified as Abstract
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Scoped
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

data InternalResult = InternalResult
  { _resultAbstract :: Abstract.AbstractResult,
    _resultModules :: NonEmpty Module,
    _resultDepInfo :: DepInfo.NameDependencyInfo
  }

makeLenses ''InternalResult

internalJuvixResultEntryPoint :: Lens' InternalResult E.EntryPoint
internalJuvixResultEntryPoint = resultAbstract . Abstract.abstractResultEntryPoint

internalJuvixResultScoped :: Lens' InternalResult Scoped.ScoperResult
internalJuvixResultScoped = resultAbstract . Abstract.resultScoper
