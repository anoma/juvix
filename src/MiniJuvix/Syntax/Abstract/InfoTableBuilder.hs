-- |

module MiniJuvix.Syntax.Abstract.InfoTableBuilder
  ( module MiniJuvix.Syntax.Abstract.InfoTableBuilder,
   module MiniJuvix.Syntax.Abstract.InfoTable)
where

import qualified Data.HashMap.Strict as HashMap
import  MiniJuvix.Syntax.Abstract.InfoTable
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name (unqualifiedSymbol)


data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef -> InfoTableBuilder m ()
  RegisterConstructor :: InductiveInfo -> InductiveConstructorDef -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef -> InfoTableBuilder m InductiveInfo
  RegisterFunction :: FunctionDef -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerFunction' ::
  Member InfoTableBuilder r =>
  FunctionDef -> Sem r FunctionDef
registerFunction' ts = registerFunction ts $> ts

registerAxiom' :: Member InfoTableBuilder r =>
  AxiomDef -> Sem r AxiomDef
registerAxiom' a = registerAxiom a $> a

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = AxiomRef (unqualifiedSymbol (d ^. axiomName))
        info = AxiomInfo {
          _axiomInfoType = d ^. axiomType,
          _axiomInfoBackends = d ^. axiomBackendItems
          }
    in modify (over infoAxioms (HashMap.insert ref info))
  RegisterConstructor _constructorInfoInductive def -> let
      ref = ConstructorRef (unqualifiedSymbol (def ^. constructorName))
      info = ConstructorInfo {
        _constructorInfoType = def ^. constructorType,
        ..
      }
      in modify (over infoConstructors (HashMap.insert ref info))
  RegisterInductive ity -> let
        ref = InductiveRef (unqualifiedSymbol (ity ^. inductiveName))
        info = InductiveInfo {
          _inductiveInfoDef = ity
        }
      in modify (over infoInductives  (HashMap.insert ref info)) $> info
  RegisterFunction _functionInfoDef -> let
      ref = FunctionRef (unqualifiedSymbol (_functionInfoDef ^. funDefName))
      info = FunctionInfo {
        ..
      }
      in modify (over infoFunctions  (HashMap.insert ref info))

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = runState emptyInfoTable . toState
