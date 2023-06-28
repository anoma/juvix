module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
  ( module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context,
    module Juvix.Compiler.Internal.Data.InfoTable,
  )
where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Concrete
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Data.NameDependencyInfo
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

-- | Top modules cache
newtype ModulesCache = ModulesCache
  {_cachedModules :: HashMap S.NameId Internal.Module}

data InternalResult = InternalResult
  { _resultScoper :: Concrete.ScoperResult,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty Module,
    _resultDepInfo :: NameDependencyInfo,
    _resultModulesCache :: ModulesCache
  }

makeLenses ''InternalResult
makeLenses ''ModulesCache

internalResultEntryPoint :: Lens' InternalResult E.EntryPoint
internalResultEntryPoint = resultScoper . Concrete.resultParserResult . Concrete.resultEntry
