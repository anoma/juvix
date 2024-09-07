module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.ResultBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra.CoercionInfo
import Juvix.Compiler.Internal.Extra.InstanceInfo
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Prelude

data ResultBuilder :: Effect where
  AddFunctionDef :: FunctionName -> Expression -> ResultBuilder m ()
  AddIdenType :: NameId -> Expression -> ResultBuilder m ()
  AddIdenTypes :: TypesTable -> ResultBuilder m ()
  AddInstanceInfo :: InstanceInfo -> ResultBuilder m ()
  AddCoercionInfo :: CoercionInfo -> ResultBuilder m ()
  LookupFunctionDef :: FunctionName -> ResultBuilder m (Maybe Expression)
  LookupIdenType :: NameId -> ResultBuilder m (Maybe Expression)
  LookupInstanceInfo :: Name -> ResultBuilder m (Maybe [InstanceInfo])
  LookupCoercionInfo :: Name -> ResultBuilder m (Maybe [CoercionInfo])
  GetCombinedInstanceTable :: ResultBuilder m InstanceTable
  GetCombinedCoercionTable :: ResultBuilder m CoercionTable

makeSem ''ResultBuilder

data ResultBuilderState = ResultBuilderState
  { _resultBuilderStateTypesTable :: TypesTable,
    _resultBuilderStateFunctionsTable :: FunctionsTable,
    _resultBuilderStateInstanceTable :: InstanceTable,
    _resultBuilderStateCoercionTable :: CoercionTable,
    _resultBuilderStateCombinedTypesTable :: TypesTable,
    _resultBuilderStateCombinedFunctionsTable :: FunctionsTable,
    _resultBuilderStateCombinedInstanceTable :: InstanceTable,
    _resultBuilderStateCombinedCoercionTable :: CoercionTable
  }

makeLenses ''ResultBuilderState

initResultBuilderState :: ImportContext -> ResultBuilderState
initResultBuilderState ctx =
  ResultBuilderState
    { _resultBuilderStateFunctionsTable = mempty,
      _resultBuilderStateTypesTable = mempty,
      _resultBuilderStateInstanceTable = mempty,
      _resultBuilderStateCoercionTable = mempty,
      _resultBuilderStateCombinedFunctionsTable = ctx ^. importContextFunctionsTable,
      _resultBuilderStateCombinedTypesTable = ctx ^. importContextTypesTable,
      _resultBuilderStateCombinedInstanceTable = ctx ^. importContextInstances,
      _resultBuilderStateCombinedCoercionTable = ctx ^. importContextCoercions
    }

evalResultBuilder :: ImportContext -> Sem (ResultBuilder ': r) a -> Sem r a
evalResultBuilder ctx = fmap snd . runResultBuilder ctx

runResultBuilder' ::
  ResultBuilderState ->
  Sem (ResultBuilder ': r) a ->
  Sem r (ResultBuilderState, a)
runResultBuilder' inis = reinterpret (runState inis) $ \case
  AddFunctionDef name def -> do
    modify' (over (resultBuilderStateFunctionsTable . functionsTable) (HashMap.insert name def))
    modify' (over (resultBuilderStateCombinedFunctionsTable . functionsTable) (HashMap.insert name def))
  AddIdenType nid ty -> do
    modify' (over (resultBuilderStateTypesTable . typesTable) (HashMap.insert nid ty))
    modify' (over (resultBuilderStateCombinedTypesTable . typesTable) (HashMap.insert nid ty))
  AddIdenTypes itab -> do
    modify' (over (resultBuilderStateTypesTable . typesTable) (HashMap.union (itab ^. typesTable)))
    modify' (over (resultBuilderStateCombinedTypesTable . typesTable) (HashMap.union (itab ^. typesTable)))
  AddInstanceInfo ii -> do
    modify' (over (resultBuilderStateInstanceTable) (flip updateInstanceTable ii))
    modify' (over (resultBuilderStateCombinedInstanceTable) (flip updateInstanceTable ii))
  AddCoercionInfo ii -> do
    modify' (over (resultBuilderStateCoercionTable) (flip updateCoercionTable ii))
    modify' (over (resultBuilderStateCombinedCoercionTable) (flip updateCoercionTable ii))
  LookupFunctionDef name ->
    gets (^. resultBuilderStateCombinedFunctionsTable . functionsTable . at name)
  LookupIdenType nid ->
    gets (^. resultBuilderStateCombinedTypesTable . typesTable . at nid)
  LookupInstanceInfo name -> do
    tab <- gets (^. resultBuilderStateCombinedInstanceTable)
    return $ lookupInstanceTable tab name
  LookupCoercionInfo name -> do
    tab <- gets (^. resultBuilderStateCombinedCoercionTable)
    return $ lookupCoercionTable tab name
  GetCombinedInstanceTable ->
    gets (^. resultBuilderStateCombinedInstanceTable)
  GetCombinedCoercionTable ->
    gets (^. resultBuilderStateCombinedCoercionTable)

runResultBuilder :: ImportContext -> Sem (ResultBuilder ': r) a -> Sem r (ResultBuilderState, a)
runResultBuilder ctx a =
  runResultBuilder' (initResultBuilderState ctx) a
