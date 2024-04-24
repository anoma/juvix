module Juvix.Compiler.Backend.Isabelle.Data.Result where

import Juvix.Compiler.Backend.Isabelle.Language

data Result = Result
  { _resultTheory :: Theory
  }

makeLenses ''Result
