module Juvix.Compiler.Backend.Lean.Data.Result where

import Juvix.Compiler.Backend.Lean.Language -- Define Lean-specific language constructs here.

data Result = Result
  { _resultModule :: Module,
    _resultModuleId :: ModuleId,
    _resultComments :: [Comment]
  }

makeLenses ''Result
