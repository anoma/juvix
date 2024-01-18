module Juvix.Compiler.Nockma.Evaluator.Options where

import Juvix.Prelude.Base

newtype EvalOptions = EvalOptions
  { _evalIgnoreStdlibCalls :: Bool
  }

defaultEvalOptions :: EvalOptions
defaultEvalOptions = EvalOptions False

makeLenses ''EvalOptions
