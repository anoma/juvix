module Juvix.Compiler.Asm.Interpreter.Error where

import Juvix.Prelude
import GHC.Show
import Control.Exception qualified as Exception

newtype RunError = RunError
  { _runErrorMsg :: Text
  }

makeLenses ''RunError

instance Show RunError where
  show :: RunError -> String
  show (RunError {..}) =
    "runtime error: "
      ++ fromText _runErrorMsg

instance Exception.Exception RunError

throwRunError :: Text -> a
throwRunError msg = Exception.throw (RunError msg)
