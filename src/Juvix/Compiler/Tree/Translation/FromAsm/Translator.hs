module Juvix.Compiler.Tree.Translation.FromAsm.Translator where

import Data.List qualified as List
import Juvix.Compiler.Asm.Extra.Base (getCommandLocation)
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Error

data Translator m a where
  NextCommand :: Translator m Command

makeSem ''Translator

newtype TranslatorState = TranslatorState
  { _stateCode :: Code
  }

makeLenses ''TranslatorState

runTranslator :: (Member (Error TreeError) r) => Code -> Sem (Translator ': r) a -> Sem r a
runTranslator cs = runTranslator' (TranslatorState cs)

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
        whenM (null <$> gets (^. stateCode)) $
          throw
            TreeError
              { _treeErrorLoc = Nothing,
                _treeErrorMsg = "expected instruction"
              }
        cmd <- gets (List.head . (^. stateCode))
        modify' (over stateCode tail)
        return cmd
