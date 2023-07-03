module Juvix.Compiler.Concrete.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef 'Scoped -> InfoTableBuilder m ()
  RegisterConstructor :: S.Symbol -> InductiveConstructorDef 'Scoped -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef 'Scoped -> InfoTableBuilder m ()
  RegisterTypeSignature :: TypeSignature 'Scoped -> InfoTableBuilder m ()
  RegisterNewTypeSignature :: NewTypeSignature 'Scoped -> InfoTableBuilder m ()
  RegisterFunctionClause :: FunctionClause 'Scoped -> InfoTableBuilder m ()
  RegisterName :: (HasLoc c) => S.Name' c -> InfoTableBuilder m ()
  RegisterModule :: Module 'Scoped 'ModuleTop -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerDoc :: Members '[HighlightBuilder] r => NameId -> Maybe (Judoc 'Scoped) -> Sem r ()
registerDoc k md = modify (set (highlightDoc . at k) md)

toState :: Members '[HighlightBuilder] r => Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = d ^. axiomName . S.nameId
        info = AxiomInfo d
        j = d ^. axiomDoc
     in do
          modify (over infoAxioms (HashMap.insert ref info))
          registerDoc (d ^. axiomName . nameId) j
  RegisterConstructor ind c ->
    let ref = c ^. constructorName . S.nameId
        info = ConstructorInfo c ind
        j = c ^. constructorDoc
     in do
          modify (over infoConstructors (HashMap.insert ref info))
          registerDoc (c ^. constructorName . nameId) j
  RegisterInductive ity ->
    let ref = ity ^. inductiveName . S.nameId
        info = InductiveInfo {_inductiveInfoDef = ity}
        j = ity ^. inductiveDoc
     in do
          modify (over infoInductives (HashMap.insert ref info))
          registerDoc (ity ^. inductiveName . nameId) j
  RegisterNewTypeSignature f ->
    let ref = f ^. signName . S.nameId
        info = FunctionInfoNew f
        j = f ^. signDoc
     in do
          modify (set (infoFunctions . at ref) (Just info))
          registerDoc (f ^. signName . nameId) j
  RegisterTypeSignature f ->
    let ref = f ^. sigName . S.nameId
        info =
          FunctionInfoOld
            OldFunctionInfo
              { _functionInfoType = f,
                _functionInfoClauses = []
              }
        j = f ^. sigDoc
     in do
          modify (set (infoFunctions . at ref) (Just info))
          registerDoc (f ^. sigName . nameId) j
  RegisterFunctionClause c ->
    -- assumes the signature has already been registered
    let key = c ^. clauseOwnerFunction . S.nameId
     in modify (over (infoFunctions . at key . _Just . _FunctionInfoOld . functionInfoClauses) (`snoc` c))
  RegisterName n -> modify (over highlightNames (cons (S.AName n)))
  RegisterModule m -> do
    let j = m ^. moduleDoc
    modify (over infoModules (HashMap.insert (m ^. modulePath) m))
    registerDoc (m ^. modulePath . nameId) j

runInfoTableBuilderRepl :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilderRepl tab = ignoreHighlightBuilder . runInfoTableBuilder tab . raiseUnder

runInfoTableBuilder :: Members '[HighlightBuilder] r => InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab = runState tab . toState

ignoreInfoTableBuilder :: Members '[HighlightBuilder] r => Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = evalState emptyInfoTable . toState
