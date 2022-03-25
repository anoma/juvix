module MiniJuvix.Syntax.Concrete.Scoped.Scoper.InfoTableBuilder where

import qualified Data.HashMap.Strict as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import MiniJuvix.Syntax.Concrete.Scoped.Name

data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef 'Scoped -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = AxiomRef' (unqualifiedSymbol (d ^. axiomName))
        info = AxiomInfo {
          _axiomInfoType = d ^. axiomType,
          _axiomInfoBackends = d ^. axiomBackendItems
          }
    in modify (over infoAxioms (HashMap.insert ref info))

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = runState emptyInfoTable . toState
