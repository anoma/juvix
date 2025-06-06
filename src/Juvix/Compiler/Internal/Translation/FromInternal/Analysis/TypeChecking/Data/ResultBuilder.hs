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
  AddPolarities :: InductiveId -> [Polarity] -> ResultBuilder m ()
  LookupFunctionDef :: FunctionName -> ResultBuilder m (Maybe Expression)
  LookupIdenType :: NameId -> ResultBuilder m (Maybe Expression)
  GetCombinedTables :: ResultBuilder m TypeCheckingTables

makeSem ''ResultBuilder

typeCheckingTablesFromImportContext :: ImportContext -> TypeCheckingTables
typeCheckingTablesFromImportContext = (^. importContextTables)

data ResultBuilderState = ResultBuilderState
  { _resultBuilderStateTables :: TypeCheckingTables,
    _resultBuilderStateCombinedTables :: TypeCheckingTables
  }

makeLenses ''ResultBuilderState

initResultBuilderState :: ImportContext -> ResultBuilderState
initResultBuilderState ctx =
  ResultBuilderState
    { _resultBuilderStateTables = mempty,
      _resultBuilderStateCombinedTables = typeCheckingTablesFromImportContext ctx
    }

runResultBuilder' ::
  ResultBuilderState ->
  Sem (ResultBuilder ': r) a ->
  Sem r (ResultBuilderState, a)
runResultBuilder' inis = reinterpret (runState inis) $ \case
  AddPolarities name pol ->
    overBothTables (set (typeCheckingTablesPolarityTable . polarityTable . at name) (Just pol))
  AddFunctionDef name def ->
    overBothTables (set (typeCheckingTablesFunctionsTable . functionsTable . at name) (Just def))
  AddIdenType nid ty ->
    overBothTables (set (typeCheckingTablesTypesTable . typesTable . at nid) (Just ty))
  AddIdenTypes itab ->
    overBothTables (over (typeCheckingTablesTypesTable . typesTable) (HashMap.union (itab ^. typesTable)))
  AddInstanceInfo ii -> do
    overBothTables (over typeCheckingTablesInstanceTable (`updateInstanceTable` ii))
  AddCoercionInfo ii ->
    overBothTables (over typeCheckingTablesCoercionTable (`updateCoercionTable` ii))
  LookupFunctionDef name ->
    gets (^. resultBuilderStateCombinedTables . typeCheckingTablesFunctionsTable . functionsTable . at name)
  LookupIdenType nid ->
    gets (^. resultBuilderStateCombinedTables . typeCheckingTablesTypesTable . typesTable . at nid)
  GetCombinedTables ->
    gets (^. resultBuilderStateCombinedTables)
  where
    overBothTables :: (Members '[State ResultBuilderState] r') => (TypeCheckingTables -> TypeCheckingTables) -> Sem r' ()
    overBothTables f = modify $ \res ->
      res
        { _resultBuilderStateTables = f (res ^. resultBuilderStateTables),
          _resultBuilderStateCombinedTables = f (res ^. resultBuilderStateCombinedTables)
        }

runResultBuilder :: ImportContext -> Sem (ResultBuilder ': r) a -> Sem r (ResultBuilderState, a)
runResultBuilder ctx a =
  runResultBuilder' (initResultBuilderState ctx) a
