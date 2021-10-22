module MiniJuvix.Syntax.Eval where

import Numeric.Natural (Natural)

import qualified MiniJuvix.Syntax.Core as Core

data Value = IsUniverse
           | IsPiType Quantity BName Value (Value -> Value)
           | IsLam BName (Value -> Value)
           | IsTensorType Quantity BName Value (Value -> Value)
           | IsTensorIntro Value Value
           | IsUnitType
           | IsUnit
           | IsSumType Value Value
           | IsInl Value
           | IsInr Value
           | IsNeutral Neutral

data Neutral = IsFree Name
             | IsApp Neutral Value
             | IsTensorTypeElim Quantity BName BName BName Neutral
                                (Value -> Value -> Value) (Value -> Value)
             | NSumElim Quantity BName Neutral BName (Value -> Value) BName
                        (Value -> Value) (Value -> Value)

valueToTerm :: Value -> Term
valueToTerm v = Checkable Unit

substCheckableTerm ::
                   CheckableTerm -> Natural -> InferableTerm -> CheckableTerm
substCheckableTerm UniverseType x m = UniverseType
substCheckableTerm (PiType q y a b) x m
  = PiType q y (substCheckableTerm a x m)
      (substCheckableTerm b (x + 1) m)
substCheckableTerm (Lam y n) x m
  = Lam y (substCheckableTerm n (x + 1) m)
substCheckableTerm (TensorType q y s t) x m
  = TensorType q y (substCheckableTerm s x m)
      (substCheckableTerm t (x + 1) m)
substCheckableTerm (TensorIntro p1 p2) x m
  = TensorIntro (substCheckableTerm p1 x m)
      (substCheckableTerm p2 x m)
substCheckableTerm UnitType x m = UnitType
substCheckableTerm Unit x m = Unit
substCheckableTerm (SumType a b) x m
  = SumType (substCheckableTerm a x m) (substCheckableTerm b x m)
substCheckableTerm (Inl n) x m = Inl (substCheckableTerm n x m)
substCheckableTerm (Inr n) x m = Inr (substCheckableTerm n x m)
substCheckableTerm (Inferred n) x m
  = Inferred (substInferableTerm n x m)

substInferableTerm ::
                   InferableTerm -> Natural -> InferableTerm -> InferableTerm
substInferableTerm (Bound y) x m = if x == y then m else Bound y
substInferableTerm (Free y) x m = Free y
substInferableTerm (Ann y a) x m
  = Ann (substCheckableTerm y x m) (substCheckableTerm a x m)
substInferableTerm (App f t) x m
  = App (substInferableTerm f x m) (substCheckableTerm t x m)
substInferableTerm (TensorTypeElim q z u v n a b) x m
  = TensorTypeElim q z u v (substInferableTerm n x m)
      (substCheckableTerm a (x + 2) m)
      (substCheckableTerm b (x + 1) m)
substInferableTerm (SumTypeElim q z esum u r1 v r2 ann) x m
  = SumTypeElim q z (substInferableTerm esum x m) u
      (substCheckableTerm r1 (x + 1) m)
      v
      (substCheckableTerm r2 (x + 1) m)
      (substCheckableTerm ann (x + 1) m)

