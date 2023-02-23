module Juvix.Compiler.Concrete.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef 'Scoped -> InfoTableBuilder m ()
  RegisterConstructor :: InductiveConstructorDef 'Scoped -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef 'Scoped -> InfoTableBuilder m ()
  RegisterFunction :: TypeSignature 'Scoped -> InfoTableBuilder m ()
  RegisterFunctionClause :: FunctionClause 'Scoped -> InfoTableBuilder m ()
  RegisterName :: (HasLoc c) => S.Name' c -> InfoTableBuilder m ()
  RegisterModule :: Module 'Scoped 'ModuleTop -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerFunction' ::
  (Member InfoTableBuilder r) =>
  TypeSignature 'Scoped ->
  Sem r (TypeSignature 'Scoped)
registerFunction' ts = registerFunction ts $> ts

registerInductive' ::
  (Member InfoTableBuilder r) =>
  InductiveDef 'Scoped ->
  Sem r (InductiveDef 'Scoped)
registerInductive' i = registerInductive i $> i

registerConstructor' ::
  (Member InfoTableBuilder r) =>
  InductiveConstructorDef 'Scoped ->
  Sem r (InductiveConstructorDef 'Scoped)
registerConstructor' c = registerConstructor c $> c

registerAxiom' ::
  (Member InfoTableBuilder r) =>
  AxiomDef 'Scoped ->
  Sem r (AxiomDef 'Scoped)
registerAxiom' a = registerAxiom a $> a

registerFunctionClause' ::
  (Member InfoTableBuilder r) =>
  FunctionClause 'Scoped ->
  Sem r (FunctionClause 'Scoped)
registerFunctionClause' a = registerFunctionClause a $> a

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = AxiomRef' (S.unqualifiedSymbol (d ^. axiomName))
        info = AxiomInfo {_axiomInfoType = d ^. axiomType}
     in modify (over infoAxioms (HashMap.insert ref info))
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
  RegisterName n -> modify (over infoNames (cons (S.AName n)))
  RegisterModule m -> modify (over infoModules (HashMap.insert (m ^. modulePath) m))

runInfoTableBuilder :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab = runState tab . toState

ignoreInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = evalState emptyInfoTable . toState
