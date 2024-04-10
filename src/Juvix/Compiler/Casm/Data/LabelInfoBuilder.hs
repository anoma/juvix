module Juvix.Compiler.Casm.Data.LabelInfoBuilder
  ( module Juvix.Compiler.Casm.Data.LabelInfo,
    module Juvix.Compiler.Casm.Data.LabelInfoBuilder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Language

data LabelInfoBuilder :: Effect where
  FreshSymbol :: LabelInfoBuilder m Symbol
  RegisterLabelName :: Symbol -> Text -> LabelInfoBuilder m ()
  RegisterLabelAddress :: Symbol -> Int -> LabelInfoBuilder m ()
  GetIdent :: Text -> LabelInfoBuilder m (Maybe Symbol)
  HasOffset :: Symbol -> LabelInfoBuilder m Bool

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

runLabelInfoBuilderWithNextId :: Word -> Sem (LabelInfoBuilder ': r) a -> Sem r (LabelInfo, a)
runLabelInfoBuilderWithNextId nextSymId =
  fmap (first (^. stateLabelInfo))
    . runLabelInfoBuilder' emptyBuilderState {_stateNextSymbolId = nextSymId}

runLabelInfoBuilder :: Sem (LabelInfoBuilder ': r) a -> Sem r (LabelInfo, a)
runLabelInfoBuilder = fmap (first (^. stateLabelInfo)) . runLabelInfoBuilder' emptyBuilderState

runLabelInfoBuilder' :: BuilderState -> Sem (LabelInfoBuilder ': r) a -> Sem r (BuilderState, a)
runLabelInfoBuilder' bs = reinterpret (runState bs) interp
  where
    interp :: LabelInfoBuilder m a -> Sem (State BuilderState ': r) a
    interp = \case
      FreshSymbol -> do
        i <- gets (^. stateNextSymbolId)
        modify' (over stateNextSymbolId (+ 1))
        return $ Symbol defaultModuleId i
      RegisterLabelName sym n ->
        modify' (over stateIdents (HashMap.insert n sym))
      RegisterLabelAddress sym addr ->
        modify' (over (stateLabelInfo . labelInfoTable) (HashMap.insert sym addr))
      GetIdent n -> do
        tab <- gets (^. stateIdents)
        return $ HashMap.lookup n tab
      HasOffset sym -> do
        tab <- gets (^. stateLabelInfo . labelInfoTable)
        return $ HashMap.member sym tab
