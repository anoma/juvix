module MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult
  ( module MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult,
    module MiniJuvix.Syntax.MonoJuvix.InfoTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.AbstractResult qualified as A
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as Micro
import MiniJuvix.Syntax.MonoJuvix.InfoTable
import MiniJuvix.Syntax.MonoJuvix.Language

type CompileInfoTable = HashMap S.NameId S.CompileInfo

data MonoJuvixResult = MonoJuvixResult
  { _resultMicroTyped :: Micro.MicroJuvixTypedResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MonoJuvixResult

compileInfoTable :: MonoJuvixResult -> CompileInfoTable
compileInfoTable r =
  HashMap.mapKeys
    (^. S.nameId)
    ( r
        ^. resultMicroTyped
          . Micro.resultMicroJuvixResult
          . Micro.resultAbstract
          . A.resultScoper
          . Scoper.resultScoperTable
          . S.infoCompilationRules
    )
