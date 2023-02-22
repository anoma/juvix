module Juvix.Compiler.Backend.Geb.Evaluator.Data.Values where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Language hiding (show)

data GebValue
  = GebValueMorphismUnit
  | GebValueMorphismInteger Integer
  | GebValueMorphismLeft GebValue
  | GebValueMorphismRight GebValue
  | GebValueMorphismPair ValueMorphismPair
  | GebValueClosure ValueClosure
  deriving stock (Show, Eq, Generic)

data ValueMorphismPair = ValueMorphismPair
  { _valueMorphismPairLeft :: GebValue,
    _valueMorphismPairRight :: GebValue
  }
  deriving stock (Show, Eq, Generic)

data ValueMorphismCase = ValueMorphismCase
  { _valueMorphismCaseOn :: GebValue,
    _valueMorphismCaseLeft :: GebValue,
    _valueMorphismCaseRight :: GebValue
  }
  deriving stock (Show, Eq, Generic)

data ValueMorphismBinop = ValueMorphismBinop
  { _valueMorphismBinopOpcode :: Opcode,
    _valueMorphismBinopLeft :: GebValue,
    _valueMorphismBinopRight :: GebValue
  }
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

makeLenses ''ValueMorphismPair
makeLenses ''ValueMorphismCase
makeLenses ''ValueMorphismBinop
makeLenses ''ValueClosure
