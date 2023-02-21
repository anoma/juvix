module Juvix.Compiler.Backend.Geb.Evaluator.Data.Values where

import Control.DeepSeq
import GHC.Show
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Language hiding (show)

data GebValue
  = GebValueMorphismUnit
  | GebValueMorphismInteger Integer
  | GebValueMorphismLeft GebValue
  | GebValueMorphismRight GebValue
  | GebValueMorphismPair ValueMorphismPair
  | GebValueClosure ValueClosure
  deriving stock (Eq, Generic)

instance Show GebValue where
  show = \case
    GebValueMorphismUnit -> "Unit"
    GebValueMorphismInteger i -> show i
    GebValueMorphismLeft l -> "Left[" <> show l <> "]"
    GebValueMorphismRight r -> "Right[" <> show r <> "]"
    GebValueMorphismPair p -> show p
    GebValueClosure c -> show c

instance NFData GebValue

data ValueMorphismPair = ValueMorphismPair
  { _valueMorphismPairLeft :: GebValue,
    _valueMorphismPairRight :: GebValue
  }
  deriving stock (Eq, Generic)

instance Show ValueMorphismPair where
  show (ValueMorphismPair l r) =
    "Pair[" <> show l <> ", " <> show r <> "]"

instance NFData ValueMorphismPair

data ValueMorphismCase = ValueMorphismCase
  { _valueMorphismCaseOn :: GebValue,
    _valueMorphismCaseLeft :: GebValue,
    _valueMorphismCaseRight :: GebValue
  }
  deriving stock (Eq, Generic)

instance Show ValueMorphismCase where
  show (ValueMorphismCase caseOn' l r) =
    "Case[on:="
      <> show caseOn'
      <> "\n"
      <> " left:="
      <> show l
      <> "\n"
      <> "  right:="
      <> show r
      <> " ]"

instance NFData ValueMorphismCase

data ValueMorphismBinop = ValueMorphismBinop
  { _valueMorphismBinopOpcode :: Opcode,
    _valueMorphismBinopLeft :: GebValue,
    _valueMorphismBinopRight :: GebValue
  }
  deriving stock (Eq, Generic)

instance Show ValueMorphismBinop where
  show (ValueMorphismBinop op l r) =
    show op <> "[ " <> show l <> " " <> show r <> " ]"

instance NFData ValueMorphismBinop

data ValueClosure = ValueClosure
  { _valueClosureEnv :: Context GebValue,
    _valueClosureLambdaBody :: Morphism
  }
  deriving stock (Eq, Generic)

instance Show ValueClosure where
  show (ValueClosure env body) =
    "Closure[ env := "
      <> show env
      <> "\n"
      <> ", body := "
      <> show body
      <> " ]"

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
