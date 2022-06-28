module MiniJuvix.Syntax.Concrete.Scoped.Scoper.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Scope

data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef 'Scoped -> InfoTableBuilder m ()
  RegisterConstructor :: InductiveConstructorDef 'Scoped -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef 'Scoped -> InfoTableBuilder m ()
  RegisterFunction :: TypeSignature 'Scoped -> InfoTableBuilder m ()
  RegisterFunctionClause :: FunctionClause 'Scoped -> InfoTableBuilder m ()
  RegisterName :: S.Name -> InfoTableBuilder m ()
  RegisterCompile :: Compile 'Scoped -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerFunction' ::
  Member InfoTableBuilder r =>
  TypeSignature 'Scoped ->
  Sem r (TypeSignature 'Scoped)
registerFunction' ts = registerFunction ts $> ts

registerInductive' ::
  Member InfoTableBuilder r =>
  InductiveDef 'Scoped ->
  Sem r (InductiveDef 'Scoped)
registerInductive' i = registerInductive i $> i

registerConstructor' ::
  Member InfoTableBuilder r =>
  InductiveConstructorDef 'Scoped ->
  Sem r (InductiveConstructorDef 'Scoped)
registerConstructor' c = registerConstructor c $> c

registerAxiom' ::
  Member InfoTableBuilder r =>
  AxiomDef 'Scoped ->
  Sem r (AxiomDef 'Scoped)
registerAxiom' a = registerAxiom a $> a

registerCompile' ::
  Member InfoTableBuilder r =>
  Compile 'Scoped ->
  Sem r (Compile 'Scoped)
registerCompile' c = registerCompile c $> c

registerFunctionClause' ::
  Member InfoTableBuilder r =>
  FunctionClause 'Scoped ->
  Sem r (FunctionClause 'Scoped)
registerFunctionClause' a = registerFunctionClause a $> a

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = AxiomRef' (S.unqualifiedSymbol (d ^. axiomName))
        info = AxiomInfo {_axiomInfoType = d ^. axiomType}
     in modify (over infoAxioms (HashMap.insert ref info))
  RegisterCompile c ->
    let symb = c ^. compileName
        info =
          CompileInfo
            { _compileInfoBackendItems = c ^. compileBackendItems,
              _compileInfoDefined = getLoc symb
            }
     in modify (over infoCompilationRules (HashMap.insert symb info))
  RegisterConstructor c ->
    let ref = ConstructorRef' (S.unqualifiedSymbol (c ^. constructorName))
        info = ConstructorInfo {_constructorInfoType = c ^. constructorType}
     in modify (over infoConstructors (HashMap.insert ref info))
  RegisterInductive ity ->
    let ref = InductiveRef' (S.unqualifiedSymbol (ity ^. inductiveName))
        info =
          InductiveInfo
            { _inductiveInfoDef = ity
            }
     in modify (over infoInductives (HashMap.insert ref info))
  RegisterFunction f ->
    let ref = FunctionRef' (S.unqualifiedSymbol (f ^. sigName))
        info =
          FunctionInfo
            { _functionInfoType = f ^. sigType
            }
     in modify (over infoFunctions (HashMap.insert ref info))
  RegisterFunctionClause c ->
    let key = c ^. clauseOwnerFunction
        value = c
     in modify (over infoFunctionClauses (HashMap.insert key value))
  RegisterName n -> modify (over infoNames (cons n))

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = runState emptyInfoTable . toState

ignoreInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = evalState emptyInfoTable . toState
