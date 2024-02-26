module Juvix.Compiler.Backend.C.Data.CBuilder where

import Juvix.Prelude

data CBuilder :: Effect where
  FreshLabel :: CBuilder m Text

makeSem ''CBuilder

newtype CBuilderState = CBuilderState
  { _stateNextLabelId :: Int
  }

makeLenses ''CBuilderState

emptyCBuilderState :: CBuilderState
emptyCBuilderState =
  CBuilderState
    { _stateNextLabelId = 0
    }

runCBuilder :: Sem (CBuilder ': r) a -> Sem r a
runCBuilder = reinterpret (evalState emptyCBuilderState) interp
  where
    interp :: CBuilder m a -> Sem (State CBuilderState ': r) a
    interp = \case
      FreshLabel -> do
        s <- get
        modify' (over stateNextLabelId (+ 1))
        return ("juvix_label_" <> show (s ^. stateNextLabelId))
