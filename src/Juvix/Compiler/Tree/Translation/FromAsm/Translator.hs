module Juvix.Compiler.Tree.Translation.FromAsm.Translator where

import Juvix.Compiler.Asm.Extra.Base (getCommandLocation)
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Error

data Translator :: Effect where
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
  (st', a) <- reinterpret (runState st) interp m
  unless (null (st' ^. stateCode))
    $ throw
      TreeError
        { _treeErrorLoc = getCommandLocation $ head' (st' ^. stateCode),
          _treeErrorMsg = "extra instructions"
        }
  return a
  where
    interp :: Translator w a' -> Sem (State TranslatorState ': r) a'
    interp = \case
      NextCommand -> do
        s <- get
        when (null (s ^. stateCode))
          $ throw
            TreeError
              { _treeErrorLoc = s ^. statePrevLoc,
                _treeErrorMsg = "expected instruction"
              }
        cmd <- gets (head' . (^. stateCode))
        modify' (over stateCode tail')
        modify' (set statePrevLoc (getCommandLocation cmd))
        return cmd
      HasNextCommand -> do
        not . null <$> gets (^. stateCode)
