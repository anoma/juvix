module Juvix.Compiler.Casm.Data.Result where

import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Language

data Result = Result
  { _resultLabelInfo :: LabelInfo,
    _resultCode :: [Instruction]
  }

makeLenses ''Result
