module Juvix.Compiler.Nockma.Evaluator.Options where

import Juvix.Prelude.Base

newtype EvalOptions = EvalOptions
  { _evalInterceptAnomaLibCalls :: Bool
  }

defaultEvalOptions :: EvalOptions
defaultEvalOptions =
  EvalOptions
    { _evalInterceptAnomaLibCalls = True
    }

makeLenses ''EvalOptions
