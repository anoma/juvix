module Juvix.Compiler.VM.Translation.FromReg.Builder where

import Juvix.Prelude

data Builder m a where
  FreshLabel :: Builder m Text

makeSem ''Builder

newtype BuilderState = BuilderState
  { _stateNextLabelId :: Int
  }

makeLenses ''BuilderState

emptyBuilderState :: BuilderState
emptyBuilderState =
  BuilderState
    { _stateNextLabelId = 0
    }

runBuilder :: Sem (Builder ': r) a -> Sem r a
runBuilder =
  evalState emptyBuilderState
    . reinterpret interp
  where
    interp :: Builder m a -> Sem (State BuilderState : r) a
    interp = \case
      FreshLabel -> do
        s <- get
        modify' (over stateNextLabelId (+ 1))
        return ("juvix_label_" <> show (s ^. stateNextLabelId))
