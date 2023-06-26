module Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context
  ( module Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context,
    module Juvix.Compiler.Abstract.Data.InfoTable,
  )
where

import Juvix.Compiler.Abstract.Data.InfoTable
import Juvix.Compiler.Abstract.Language
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Concrete
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude

newtype ModulesCache = ModulesCache
  {_cachedModules :: HashMap S.NameId Abstract.TopModule}

data AbstractResult = AbstractResult
  { _resultScoper :: Concrete.ScoperResult,
    _resultTable :: InfoTable,
    _resultModules :: NonEmpty TopModule,
    _resultModulesCache :: ModulesCache
  }

makeLenses ''AbstractResult
makeLenses ''ModulesCache

abstractResultEntryPoint :: Lens' AbstractResult E.EntryPoint
abstractResultEntryPoint = resultScoper . Concrete.resultParserResult . Concrete.resultEntry
