module Juvix.Compiler.Backend.Geb.Evaluator.Data.RunEvalResult (module Juvix.Compiler.Backend.Geb.Evaluator.Data.RunEvalResult) where

import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Language

data RunEvalResult
  = RunEvalResultGebValue GebValue
  | RunEvalResultMorphism Morphism
  deriving stock (Show, Eq)

instance HasAtomicity RunEvalResult where
  atomicity (RunEvalResultGebValue v) = atomicity v
  atomicity (RunEvalResultMorphism m) = atomicity m
