module Juvix.Compiler.Backend.Geb.Evaluator.Data.Values where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Language hiding (show)

type ValueLeftInj = LeftInj' GebValue

type ValueRightInj = RightInj' GebValue

type ValuePair = Pair' GebValue

data GebValue
  = GebValueMorphismUnit
  | GebValueMorphismInteger Integer
  | GebValueMorphismLeft ValueLeftInj
  | GebValueMorphismRight ValueRightInj
  | GebValueMorphismPair ValuePair
  | GebValueClosure ValueClosure
  deriving stock (Show, Eq, Generic)

data ValueClosure = ValueClosure
  { _valueClosureEnv :: Context GebValue,
    _valueClosureLambda :: Lambda
  }
  deriving stock (Show, Eq, Generic)

instance HasAtomicity GebValue where
  atomicity = \case
    GebValueMorphismInteger {} -> Atom
    GebValueMorphismLeft {} -> Aggregate appFixity
    GebValueMorphismPair {} -> Aggregate appFixity
    GebValueMorphismRight {} -> Aggregate appFixity
    GebValueMorphismUnit -> Atom
    GebValueClosure {} -> Aggregate appFixity

makeLenses ''ValueClosure
