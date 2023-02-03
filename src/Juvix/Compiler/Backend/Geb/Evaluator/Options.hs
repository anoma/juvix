module Juvix.Compiler.Backend.Geb.Evaluator.Options where

import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Prelude

{-
  Evaluation strategies here: call-by-value (eager evaluation) vs call-by-name
  (lazy evaluation). Call-by-value evaluates arguments before function is
  applied, call-by-name evaluates arguments only when used in function body.
-}

data EvalStrategy = Full | CallByName | CallByValue
  deriving stock (Show, Data)

defaultEvalStrategy :: EvalStrategy
defaultEvalStrategy = CallByValue

newtype EvaluatorOptions = EvaluatorOptions
  { _evaluatorOptionsEvalStrategy :: EvalStrategy
  }

makeLenses ''EvaluatorOptions

instance CanonicalProjection EvaluatorOptions Geb.Options where
  project _ = Geb.defaultOptions

defaultEvaluatorOptions :: EvaluatorOptions
defaultEvaluatorOptions =
  EvaluatorOptions
    { _evaluatorOptionsEvalStrategy =
        defaultEvalStrategy
    }
