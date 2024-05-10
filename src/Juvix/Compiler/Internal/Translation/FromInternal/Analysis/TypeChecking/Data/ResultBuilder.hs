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
  AddInstanceInfo :: InstanceInfo -> ResultBuilder m ()
  AddCoercionInfo :: CoercionInfo -> ResultBuilder m ()
  LookupFunctionDef' :: FunctionName -> ResultBuilder m (Maybe Expression)
  LookupIdenType' :: NameId -> ResultBuilder m (Maybe Expression)
  LookupInstanceInfo' :: Name -> ResultBuilder m (Maybe [InstanceInfo])
  LookupCoercionInfo' :: Name -> ResultBuilder m (Maybe [CoercionInfo])

makeSem ''ResultBuilder

data ResultBuilderState = ResultBuilderState
  { _resultBuilderStateTypesTable :: TypesTable,
    _resultBuilderStateFunctionsTable :: FunctionsTable,
    _resultBuilderStateInstanceTable :: InstanceTable,
    _resultBuilderStateCoercionTable :: CoercionTable
  }

makeLenses ''ResultBuilderState

emptyResultBuilderState :: ResultBuilderState
emptyResultBuilderState =
  ResultBuilderState
    { _resultBuilderStateFunctionsTable = mempty,
      _resultBuilderStateTypesTable = mempty,
      _resultBuilderStateInstanceTable = mempty,
      _resultBuilderStateCoercionTable = mempty
    }

lookupFunctionDef :: (Members '[ResultBuilder, Reader ImportContext] r) => FunctionName -> Sem r (Maybe Expression)
lookupFunctionDef name = do
  m <- lookupFunctionDef' name
  case m of
    Just def -> return $ Just def
    Nothing -> asks (^. importContextFunctionsTable . functionsTable . at name)

lookupIdenType :: (Members '[ResultBuilder, Reader ImportContext] r) => NameId -> Sem r (Maybe Expression)
lookupIdenType nid = do
  m <- lookupIdenType' nid
  case m of
    Just def -> return $ Just def
    Nothing -> asks (^. importContextTypesTable . typesTable . at nid)

combineMaybeInfos :: Maybe [a] -> Maybe [a] -> Maybe [a]
combineMaybeInfos is1 is2 = case (is1, is2) of
  (Nothing, Nothing) -> Nothing
  (Just is1', Nothing) -> Just is1'
  (Nothing, Just is2') -> Just is2'
  (Just is1', Just is2') -> Just (is1' ++ is2')

lookupInstanceInfo :: (Members '[ResultBuilder, Reader ImportContext] r) => Name -> Sem r (Maybe [InstanceInfo])
lookupInstanceInfo name = do
  is1 <- lookupInstanceInfo' name
  tab <- asks (^. importContextInfoTable . infoInstances)
  let is2 = lookupInstanceTable tab name
  return $ combineMaybeInfos is1 is2

lookupCoercionInfo :: (Members '[ResultBuilder, Reader ImportContext] r) => Name -> Sem r (Maybe [CoercionInfo])
lookupCoercionInfo name = do
  is1 <- lookupCoercionInfo' name
  tab <- asks (^. importContextInfoTable . infoCoercions)
  let is2 = lookupCoercionTable tab name
  return $ combineMaybeInfos is1 is2

runResultBuilder' ::
  ResultBuilderState ->
  Sem (ResultBuilder ': r) a ->
  Sem r (ResultBuilderState, a)
runResultBuilder' inis = reinterpret (runState inis) $ \case
  AddFunctionDef name def ->
    modify' (over (resultBuilderStateFunctionsTable . functionsTable) (HashMap.insert name def))
  AddIdenType nid ty ->
    modify' (over (resultBuilderStateTypesTable . typesTable) (HashMap.insert nid ty))
  AddInstanceInfo ii ->
    modify' (over (resultBuilderStateInstanceTable) (flip updateInstanceTable ii))
  AddCoercionInfo ii ->
    modify' (over (resultBuilderStateCoercionTable) (flip updateCoercionTable ii))
  LookupFunctionDef' name ->
    gets (^. resultBuilderStateFunctionsTable . functionsTable . at name)
  LookupIdenType' nid ->
    gets (^. resultBuilderStateTypesTable . typesTable . at nid)
  LookupInstanceInfo' name -> do
    tab <- gets (^. resultBuilderStateInstanceTable)
    return $ lookupInstanceTable tab name
  LookupCoercionInfo' name -> do
    tab <- gets (^. resultBuilderStateCoercionTable)
    return $ lookupCoercionTable tab name

runResultBuilder :: Sem (ResultBuilder ': r) a -> Sem r (ResultBuilderState, a)
runResultBuilder = runResultBuilder' emptyResultBuilderState
