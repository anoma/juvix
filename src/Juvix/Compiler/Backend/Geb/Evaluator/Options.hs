module Juvix.Compiler.Backend.Geb.Evaluator.Options where

import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Prelude

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
