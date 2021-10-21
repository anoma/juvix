module MiniJuvix.Syntax.Eval where

--------------------------------------------------------------------------------

open import Haskell.Prelude
open import Agda.Builtin.Equality

open import MiniJuvix.Syntax.Core

{-# FOREIGN AGDA2HS
import qualified MiniJuvix.Syntax.Core as Core
#-}

--------------------------------------------------------------------------------

-- What is a value? it's simply a term that contains an answer, i.e. a
-- term that does no longer reduce.

data Value : Set where
  Universe : Value
  -- TODO: Obviously there is more stuff I need to add here to cover
  -- all the possible values.

{-# COMPILE AGDA2HS Value #-}

--------------------------------------------------------------------------------
-- Substitution.
--------------------------------------------------------------------------------

substCheckableTerm
  : CheckableTerm    -- Term N
  → Nat              -- Bound variable x
  -> InferableTerm   -- M
  -> CheckableTerm   -- N[x := M]

substInferableTerm
  : InferableTerm    -- Term N
  →  Nat             -- bound variable x (Bruijn)
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
substInferableTerm (Bound y) x m = if x == y then m else Bound y
substInferableTerm (Free y) x m = Free y
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