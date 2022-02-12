module MiniJuvix.Syntax.Eval where

--------------------------------------------------------------------------------

open import Haskell.Prelude
open import Agda.Builtin.Equality

open import MiniJuvix.Syntax.Core

--------------------------------------------------------------------------------
-- Haskell stuff
--------------------------------------------------------------------------------

{-# FOREIGN AGDA2HS
{-# OPTIONS_GHC -fno-warn-missing-export-lists -fno-warn-unused-matches #-}
#-}

{-# FOREIGN AGDA2HS
import MiniJuvix.Syntax.Core
import MiniJuvix.Prelude
#-}

{-# FOREIGN AGDA2HS
--------------------------------------------------------------------------------
-- Values and neutral terms
--------------------------------------------------------------------------------
#-}

{-
  We are interested in a normal form for posibbly open terms. This
  means that a term may have free variables. Therefore, we must
  consider two kind of reduced terms: values and neutral terms. A term
  that do not longer beta-reduce (i.e. that it contains an evaluation
  answer) is called a value. A term is neutral whenever its futher
  beta-reduction depends on a free variable. Terms in normal form are
  then defined by mutual recursion with neutral terms.
-}

{- Since Agda2HS does not support indexed data types, we have to
  repeat ourselves with syntax for values and neutral terms based on
  Core syntax. The following is ideally for formal verification, but
  not doable.
-}

{-
data Value : Term → Set
data Neutral : Term → Set

data Value where
  IsUniverse : Value (Checkable UniverseType)
  IsNeutral : (t : Term) → Neutral t → Value t
  IsUnit : Value (Checkable Unit)
  IsUnitType : Value (Checkable UnitType)
  ...

data Neutral where
  IsFree : (b : Name) → Neutral (Inferable (Free b))
  ...
-}

{-# NO_POSITIVITY_CHECK #-}
data Value : Set
data Neutral : Set

data Value where
  IsUniverse : Value
  IsPiType : Quantity → BindingName → Value → (Value -> Value) -> Value
  IsLam : BindingName → (Value -> Value) -> Value
  IsTensorType : Quantity → BindingName → Value → (Value -> Value) -> Value
  IsTensorIntro : Value → Value -> Value
  IsUnitType : Value
  IsUnit : Value
  IsSumType : Value → Value -> Value
  IsInl : Value -> Value
  IsInr : Value -> Value
  IsNeutral : Neutral -> Value

{-# COMPILE AGDA2HS Value #-}

data Neutral where
  IsFree : Name → Neutral
  IsApp : Neutral → Value → Neutral
  IsTensorTypeElim :
    Quantity → BindingName → BindingName → BindingName
    → Neutral
    → (Value -> Value -> Value)
    → (Value -> Value)
    → Neutral
  NSumElim :
    Quantity
    → BindingName
    → Neutral
    → BindingName
    → (Value -> Value)
    → BindingName
    → (Value -> Value)
    → (Value -> Value)
    → Neutral

{-# COMPILE AGDA2HS Neutral #-}

-- We can have an embedding from values to terms as a sort of quoting.
-- Usages: we can check for value equality by defining term equality.

valueToTerm : Value → Term
valueToTerm v = Checkable Unit -- TODO
{-# COMPILE AGDA2HS valueToTerm #-}

--------------------------------------------------------------------------------
-- Substitution of bound variables. Recall a bound variable is
-- constructed using Bound and a natural number. The following is one
-- special case of substitution. See a relevant discussion on
-- ekmett/bound-making-de-bruijn-succ-less. For QTT, see Def. 12 in
-- Conor's paper.
--------------------------------------------------------------------------------

substCheckableTerm
  : CheckableTerm    -- Term N
  → Index            -- Bound variable x
  -> InferableTerm   -- M
  -> CheckableTerm   -- N[x := M]

substInferableTerm
  : InferableTerm    -- Term N
  →  Index           -- bound variable x
  -> InferableTerm   -- inferable term M
  -> InferableTerm   -- N[x := M]

substCheckableTerm UniverseType x m = UniverseType
substCheckableTerm (PiType q y a b) x m
  = PiType q y
      (substCheckableTerm a x m)
      (substCheckableTerm b (x + 1) m)
substCheckableTerm (Lam y n) x m
  = Lam y (substCheckableTerm n (x + 1) m)
substCheckableTerm (TensorType q y s t) x m
  = TensorType q y
      (substCheckableTerm s x m)
      (substCheckableTerm t (x + 1) m)
substCheckableTerm (TensorIntro p1 p2) x m
  = TensorIntro
      (substCheckableTerm p1 x m)
      (substCheckableTerm p2 x m)
substCheckableTerm UnitType x m = UnitType
substCheckableTerm Unit x m = Unit
substCheckableTerm (SumType a b) x m
  = SumType
      (substCheckableTerm a x m)
      (substCheckableTerm b x m)
substCheckableTerm (Inl n) x m = Inl (substCheckableTerm n x m)
substCheckableTerm (Inr n) x m = Inr (substCheckableTerm n x m)
substCheckableTerm (Inferred n) x m
  = Inferred (substInferableTerm n x m)
{-# COMPILE AGDA2HS substCheckableTerm #-}

-- Variable substitution.
substInferableTerm (Var (Bound y)) x m = if x == y then m else Var (Bound y)
substInferableTerm (Var (Free y)) x m  = Var (Free y)
-- we subst. checkable parts.
substInferableTerm (Ann y a) x m
  = Ann (substCheckableTerm y x m)
        (substCheckableTerm a x m)
substInferableTerm (App f t) x m
  = App (substInferableTerm f x m)
        (substCheckableTerm t x m)
substInferableTerm (TensorTypeElim q z u v n a b) x m
  = TensorTypeElim q z u v
      (substInferableTerm n x m)
      (substCheckableTerm a (x + 2) m)
      (substCheckableTerm b (x + 1) m)
substInferableTerm (SumTypeElim q z esum u r1 v r2 ann) x m
  = SumTypeElim q z
    (substInferableTerm esum x m)
    u (substCheckableTerm r1 (x + 1) m)
    v (substCheckableTerm r2 (x + 1) m)
    (substCheckableTerm ann (x + 1) m)
{-# COMPILE AGDA2HS substInferableTerm #-}

{-- Substitution, denoted by (N[x := M]) is defined by mutual
  recursion and by induction on N, and replace all the ocurrences of
  'x' by M in the term N. Recall that N is a term of type either
  CheckableTerm or InferableTerm.
-}

substTerm : Term → Nat → InferableTerm → Term
substTerm (Checkable n) x m = Checkable (substCheckableTerm n x m)
substTerm (Inferable n) x m = Inferable (substInferableTerm n x m)
