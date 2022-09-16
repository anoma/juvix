module Juvix.Compiler.Asm.Interpreter.Error where

import Control.Exception qualified as Exception
import GHC.Show
import Juvix.Prelude

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
