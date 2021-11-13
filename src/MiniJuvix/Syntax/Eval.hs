{-# OPTIONS_GHC
  -fno-warn-missing-export-lists -fno-warn-unused-matches #-}

module MiniJuvix.Syntax.Eval where

import MiniJuvix.Syntax.Core
import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------
-- Values and neutral terms
--------------------------------------------------------------------------------

data Value = IsUniverse
           | IsPiType Quantity BindingName Value (Value -> Value)
           | IsLam BindingName (Value -> Value)
           | IsTensorType Quantity BindingName Value (Value -> Value)
           | IsTensorIntro Value Value
           | IsUnitType
           | IsUnit
           | IsSumType Value Value
           | IsInl Value
           | IsInr Value
           | IsNeutral Neutral

data Neutral = IsFree Name
             | IsApp Neutral Value
             | IsTensorTypeElim Quantity BindingName BindingName BindingName
                                Neutral (Value -> Value -> Value) (Value -> Value)
             | NSumElim Quantity BindingName Neutral BindingName
                        (Value -> Value) BindingName (Value -> Value) (Value -> Value)

valueToTerm :: Value -> Term
valueToTerm v = Checkable Unit

substCheckableTerm ::
                   CheckableTerm -> Index -> InferableTerm -> CheckableTerm
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
                   InferableTerm -> Index -> InferableTerm -> InferableTerm
substInferableTerm (Var (Bound y)) x m
  = if x == y then m else Var (Bound y)
substInferableTerm (Var (Free y)) x m = Var (Free y)
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

