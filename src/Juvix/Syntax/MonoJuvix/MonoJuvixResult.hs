module Juvix.Syntax.MonoJuvix.MonoJuvixResult
  ( module Juvix.Syntax.MonoJuvix.MonoJuvixResult,
    module Juvix.Syntax.MonoJuvix.InfoTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Prelude
import Juvix.Syntax.Abstract.AbstractResult qualified as Abstract
import Juvix.Syntax.Concrete.Scoped.InfoTable qualified as Scoper
import Juvix.Syntax.Concrete.Scoped.Name qualified as Scoper
import Juvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import Juvix.Syntax.MicroJuvix.MicroJuvixArityResult qualified as Micro
import Juvix.Syntax.MicroJuvix.MicroJuvixResult qualified as Micro
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as Micro
import Juvix.Syntax.MonoJuvix.InfoTable
import Juvix.Syntax.MonoJuvix.Language

type CompileInfoTable = HashMap Scoper.NameId Scoper.CompileInfo

data MonoJuvixResult = MonoJuvixResult
  { _resultMicroTyped :: Micro.MicroJuvixTypedResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MonoJuvixResult

compileInfoTable :: MonoJuvixResult -> CompileInfoTable
compileInfoTable r =
  HashMap.mapKeys
    (^. Scoper.nameId)
    ( r
        ^. resultMicroTyped
        . Micro.resultMicroJuvixArityResult
        . Micro.resultMicroJuvixResult
        . Micro.resultAbstract
        . Abstract.resultScoper
        . Scoper.resultScoperTable
        . Scoper.infoCompilationRules
    )
