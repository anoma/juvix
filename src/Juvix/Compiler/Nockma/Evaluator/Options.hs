module Juvix.Compiler.Nockma.Evaluator.Options where

import Juvix.Prelude.Base

newtype EvalOptions = EvalOptions
  { _evalInterceptStdlibCalls :: Bool
  }

defaultEvalOptions :: EvalOptions
defaultEvalOptions =
  EvalOptions
    { _evalInterceptStdlibCalls = True
    }

makeLenses ''EvalOptions
