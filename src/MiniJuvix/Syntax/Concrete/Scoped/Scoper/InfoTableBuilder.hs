module MiniJuvix.Syntax.Concrete.Scoped.Scoper.InfoTableBuilder where

import qualified Data.HashMap.Strict as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import MiniJuvix.Syntax.Concrete.Scoped.Name

data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef 'Scoped -> InfoTableBuilder m ()
  RegisterConstructor :: InductiveConstructorDef 'Scoped -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef 'Scoped -> InfoTableBuilder m ()
  RegisterFunction :: TypeSignature 'Scoped -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerFunction' ::
  Member InfoTableBuilder r =>
  TypeSignature 'Scoped -> Sem r (TypeSignature 'Scoped)
registerFunction' ts = registerFunction ts $> ts

registerInductive' :: Member InfoTableBuilder r =>
  InductiveDef 'Scoped -> Sem r (InductiveDef 'Scoped)
registerInductive' i = registerInductive i $> i

registerConstructor' :: Member InfoTableBuilder r =>
  InductiveConstructorDef 'Scoped -> Sem r (InductiveConstructorDef 'Scoped)
registerConstructor' c = registerConstructor c $> c

registerAxiom' :: Member InfoTableBuilder r =>
  AxiomDef 'Scoped -> Sem r (AxiomDef 'Scoped)
registerAxiom' a = registerAxiom a $> a

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = AxiomRef' (unqualifiedSymbol (d ^. axiomName))
        info = AxiomInfo {
          _axiomInfoType = d ^. axiomType,
          _axiomInfoBackends = d ^. axiomBackendItems
          }
    in modify (over infoAxioms (HashMap.insert ref info))
  RegisterConstructor c -> let
      ref = ConstructorRef' (unqualifiedSymbol (c ^. constructorName))
      info = ConstructorInfo {
        _constructorInfoType = c ^. constructorType
      }
      in modify (over infoConstructors  (HashMap.insert ref info)
   )
  RegisterInductive ity -> let
        ref = InductiveRef' (unqualifiedSymbol (ity ^. inductiveName))
        info = InductiveInfo {
          _inductiveInfoDef = ity
        }
      in modify (over infoInductives  (HashMap.insert ref info)
   )
  RegisterFunction f -> let
      ref = FunctionRef' (unqualifiedSymbol (f ^. sigName))
      info = FunctionInfo {
        _functionInfoType = f ^. sigType
      }
      in modify (over infoFunctions  (HashMap.insert ref info)
   )

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = runState emptyInfoTable . toState
