module Juvix.Compiler.Backend.Geb.Evaluator.Options where

import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Prelude

newtype EvaluatorOptions = EvaluatorOptions
  { _evaluatorOptionsOutputMorphism :: Bool
  }

makeLenses ''EvaluatorOptions

instance CanonicalProjection EvaluatorOptions Geb.Options where
  project _ = Geb.defaultOptions

defaultEvaluatorOptions :: EvaluatorOptions
defaultEvaluatorOptions =
  EvaluatorOptions
    { _evaluatorOptionsOutputMorphism = False
    }
