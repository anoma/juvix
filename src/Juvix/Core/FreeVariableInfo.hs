module Juvix.Core.FreeVariableInfo where

import Juvix.Core.Language
import Juvix.Prelude

newtype FreeVariableInfo = FreeVariableInfo
  { _freeVars :: HashSet Index
  }

