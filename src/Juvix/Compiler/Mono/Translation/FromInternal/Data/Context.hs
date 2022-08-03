module Juvix.Compiler.Mono.Translation.FromInternal.Data.Context
  ( module Juvix.Compiler.Mono.Translation.FromInternal.Data.Context,
    module Juvix.Compiler.Mono.Data.InfoTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context qualified as Abstract
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Data.ScopedName qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Internal.Translation.FromAbstract qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as ArityChecking
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as TypeChecking
import Juvix.Compiler.Mono.Data.InfoTable
import Juvix.Compiler.Mono.Language
import Juvix.Prelude

type CompileInfoTable = HashMap Scoper.NameId Scoper.CompileInfo

data MonoJuvixResult = MonoJuvixResult
  { _resultMicroTyped :: TypeChecking.InternalTypedResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MonoJuvixResult

compileInfoTable :: MonoJuvixResult -> CompileInfoTable
compileInfoTable r =
  HashMap.mapKeys
    (^. Scoper.nameId)
    ( r
        ^. resultMicroTyped
        . TypeChecking.resultInternalArityResult
        . ArityChecking.resultInternalResult
        . Internal.resultAbstract
        . Abstract.resultScoper
        . Scoper.resultScoperTable
        . Scoper.infoCompilationRules
    )
