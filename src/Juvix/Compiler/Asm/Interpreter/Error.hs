module Juvix.Compiler.Asm.Interpreter.Error where

import Control.Exception qualified as Exception
import GHC.Show
import Juvix.Compiler.Asm.Interpreter.RuntimeState

data RunError = RunError
  { _runErrorMsg :: Text,
    _runErrorState :: RuntimeState
  }

makeLenses ''RunError

instance Show RunError where
  show :: RunError -> String
  show (RunError {..}) =
    "runtime error: "
      ++ fromText _runErrorMsg

instance Exception.Exception RunError

throwRunError :: RuntimeState -> Text -> a
throwRunError st msg = Exception.throw (RunError msg st)
