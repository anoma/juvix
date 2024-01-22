module Juvix.Compiler.Tree.Translation.FromAsm.Translator where

import Data.List qualified as List
import Juvix.Compiler.Asm.Extra.Base (getCommandLocation)
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Error

data Translator m a where
  NextCommand :: Translator m Command
  HasNextCommand :: Translator m Bool

makeSem ''Translator

data TranslatorState = TranslatorState
  { _stateCode :: Code,
    _statePrevLoc :: Maybe Location
  }

makeLenses ''TranslatorState

runTranslator :: (Member (Error TreeError) r) => Code -> Sem (Translator ': r) a -> Sem r a
runTranslator cs = runTranslator' (TranslatorState cs Nothing)

runTranslator' :: forall r a. (Member (Error TreeError) r) => TranslatorState -> Sem (Translator ': r) a -> Sem r a
runTranslator' st m = do
  (st', a) <- runState st $ reinterpret interp m
  unless (null (st' ^. stateCode)) $
    throw
      TreeError
        { _treeErrorLoc = getCommandLocation $ List.head (st' ^. stateCode),
          _treeErrorMsg = "extra instructions"
        }
  return a
  where
    interp :: Translator m a' -> Sem (State TranslatorState ': r) a'
    interp = \case
      NextCommand -> do
        s <- get
        when (null (s ^. stateCode)) $
          throw
            TreeError
              { _treeErrorLoc = s ^. statePrevLoc,
                _treeErrorMsg = "expected instruction"
              }
        cmd <- gets (List.head . (^. stateCode))
        modify' (over stateCode tail)
        modify' (set statePrevLoc (getCommandLocation cmd))
        return cmd
      HasNextCommand -> do
        not . null <$> gets (^. stateCode)
