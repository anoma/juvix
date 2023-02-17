module Juvix.Compiler.Backend.Geb.Evaluator.Data.Values where

import Control.DeepSeq
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Language

data GebValue
  = GebValueMorphismUnit
  | GebValueMorphismInteger Integer
  | GebValueMorphismLeft GebValue
  | GebValueMorphismRight GebValue
  | GebValueMorphismPair ValueMorphismPair
  | GebValueClosure ValueClosure
  deriving stock (Show, Eq, Generic)

instance NFData GebValue

data ValueMorphismPair = ValueMorphismPair
  { _valueMorphismPairLeft :: GebValue,
    _valueMorphismPairRight :: GebValue
  }
  deriving stock (Show, Eq, Generic)

instance NFData ValueMorphismPair

data ValueMorphismCase = ValueMorphismCase
  { _valueMorphismCaseOn :: GebValue,
    _valueMorphismCaseLeft :: GebValue,
    _valueMorphismCaseRight :: GebValue
  }
  deriving stock (Show, Eq, Generic)

instance NFData ValueMorphismCase

data ValueMorphismBinop = ValueMorphismBinop
  { _valueMorphismBinopOpcode :: Opcode,
    _valueMorphismBinopLeft :: GebValue,
    _valueMorphismBinopRight :: GebValue
  }
  deriving stock (Show, Eq, Generic)

instance NFData ValueMorphismBinop

data ValueClosure = ValueClosure
  { _valueClosureEnv :: Context GebValue,
    _valueClosureLambdaBody :: Morphism
  }
  deriving stock (Show, Eq, Generic)

instance NFData ValueClosure

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
