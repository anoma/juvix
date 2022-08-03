module Juvix.Asm.Translation.Extra.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Asm.Data.InfoTable
import Juvix.Asm.Language

data InfoTableBuilder m a where
  RegisterFunction :: FunctionInfo -> InfoTableBuilder m ()
  RegisterConstr :: ConstrInfo -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = runState emptyInfoTable . toState
  where
    toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
    toState = reinterpret $ \case
      RegisterFunction fi ->
        modify (over infoFunctions (HashMap.insert (fi ^. functionInfoSymbol) fi))
      RegisterConstr ci ->
        modify (over infoConstrs (HashMap.insert (ci ^. constrInfoTag) ci))
