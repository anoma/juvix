module Juvix.Compiler.Concrete.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName
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

registerDoc :: NameId -> Maybe (Judoc 'Scoped) -> Sem (State InfoTable ': r) ()
registerDoc k md = modify (set (infoDoc . at k) md)

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = AxiomRef' (S.unqualifiedSymbol (d ^. axiomName))
        info = AxiomInfo {_axiomInfoType = d ^. axiomType}
        j = d ^. axiomDoc
     in do
          modify (over infoAxioms (HashMap.insert ref info))
          registerDoc (d ^. axiomName . nameId) j
  RegisterConstructor c ->
    let ref = ConstructorRef' (S.unqualifiedSymbol (c ^. constructorName))
        info = ConstructorInfo {_constructorInfoType = c ^. constructorType}
        j = c ^. constructorDoc
     in do
          modify (over infoConstructors (HashMap.insert ref info))
          registerDoc (c ^. constructorName . nameId) j
  RegisterInductive ity ->
    let ref = InductiveRef' (S.unqualifiedSymbol (ity ^. inductiveName))
        info = InductiveInfo {_inductiveInfoDef = ity}
        j = ity ^. inductiveDoc
     in do
          modify (over infoInductives (HashMap.insert ref info))
          registerDoc (ity ^. inductiveName . nameId) j
  RegisterFunction f ->
    let ref = FunctionRef' (S.unqualifiedSymbol (f ^. sigName))
        info = FunctionInfo {_functionInfoType = f ^. sigType}
        j = f ^. sigDoc
     in do
          modify (over infoFunctions (HashMap.insert ref info))
          registerDoc (f ^. sigName . nameId) j
  RegisterFunctionClause c ->
    let key = c ^. clauseOwnerFunction
        value = c
     in do modify (over infoFunctionClauses (HashMap.insert key value))
  RegisterName n -> modify (over infoNames (cons (S.AName n)))
  RegisterModule m -> do
    let j = m ^. moduleDoc
    modify (over infoModules (HashMap.insert (m ^. modulePath) m))
    registerDoc (m ^. modulePath . nameId) j

runInfoTableBuilder :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab = runState tab . toState

ignoreInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = evalState emptyInfoTable . toState
