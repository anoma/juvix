module Juvix.Compiler.Backend.Isabelle.Data.Result where

import Juvix.Compiler.Backend.Isabelle.Language

data Result = Result
  { _resultTheory :: Theory,
    _resultModuleId :: ModuleId
  }

makeLenses ''Result
