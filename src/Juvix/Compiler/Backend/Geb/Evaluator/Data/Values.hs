module Juvix.Compiler.Backend.Geb.Evaluator.Data.Values where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Language hiding (show)

data GebValue
  = GebValueMorphismUnit
  | GebValueMorphismInteger Integer
  | GebValueMorphismLeft (LeftInj' GebValue)
  | GebValueMorphismRight (RightInj' GebValue)
  | GebValueMorphismPair (Pair' GebValue)
  | GebValueClosure ValueClosure
  deriving stock (Show, Eq, Generic)

data ValueClosure = ValueClosure
  { _valueClosureEnv :: Context GebValue,
    _valueClosureLambdaBody :: Morphism
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
