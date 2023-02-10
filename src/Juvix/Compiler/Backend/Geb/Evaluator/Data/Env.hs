module Juvix.Compiler.Backend.Geb.Evaluator.Data.Env where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language

data Env = Env
  { _envEvaluatorOptions :: EvaluatorOptions,
    _envContext :: Context GebValue
  }

makeLenses ''Env

defaultEvalEnv :: Env
defaultEvalEnv =
  Env
    { _envEvaluatorOptions = defaultEvaluatorOptions,
      _envContext = Context.empty
    }
