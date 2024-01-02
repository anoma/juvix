module Juvix.Compiler.Casm.Data.LabelInfoBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Language

data LabelInfoBuilder m a where
  FreshSymbol :: LabelInfoBuilder m Symbol
  RegisterLabelName :: Symbol -> Text -> LabelInfoBuilder m ()
  RegisterLabelOffset :: Symbol -> Offset -> LabelInfoBuilder m ()
  GetIdent :: Text -> LabelInfoBuilder m (Maybe Symbol)

makeSem ''LabelInfoBuilder

data BuilderState = BuilderState
  { _stateNextSymbolId :: Word,
    _stateLabelInfo :: LabelInfo,
    _stateIdents :: HashMap Text Symbol
  }

makeLenses ''BuilderState

emptyBuilderState :: BuilderState
emptyBuilderState =
  BuilderState
    { _stateNextSymbolId = 0,
      _stateLabelInfo = mempty,
      _stateIdents = mempty
    }

runLabelInfoBuilder :: Sem (LabelInfoBuilder ': r) a -> Sem r (LabelInfo, a)
runLabelInfoBuilder = fmap (first (^. stateLabelInfo)) . runLabelInfoBuilder' emptyBuilderState

runLabelInfoBuilder' :: BuilderState -> Sem (LabelInfoBuilder ': r) a -> Sem r (BuilderState, a)
runLabelInfoBuilder' bs =
  runState bs
    . reinterpret interp
  where
    interp :: LabelInfoBuilder m a -> Sem (State BuilderState ': r) a
    interp = \case
      FreshSymbol -> do
        i <- gets (^. stateNextSymbolId)
        modify' (over stateNextSymbolId (+ 1))
        return $ Symbol defaultModuleId i
      RegisterLabelName sym n ->
        modify' (over stateIdents (HashMap.insert n sym))
      RegisterLabelOffset sym off ->
        modify' (over (stateLabelInfo . labelInfoTable) (HashMap.insert sym off))
      GetIdent n -> do
        tab <- gets (^. stateIdents)
        return $ HashMap.lookup n tab
