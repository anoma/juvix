module Juvix.Compiler.Backend.C.Data.CBuilder where

import Juvix.Prelude

data CBuilder m a where
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
runCBuilder =
  evalState emptyCBuilderState
    . reinterpret interp
  where
    interp :: CBuilder m a -> Sem (State CBuilderState ': r) a
    interp = \case
      FreshLabel -> do
        s <- get
        modify' (over stateNextLabelId (+ 1))
        return ("juvix_label_" <> show (s ^. stateNextLabelId))
