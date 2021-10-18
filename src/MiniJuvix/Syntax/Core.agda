module MiniJuvix.Syntax.Core where

open import Haskell.Prelude
open import Agda.Builtin.Equality

-- language extensions
{-# FOREIGN AGDA2HS
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
#-}

data Quantity : Set where
  Zero One Many : Quantity

{-# COMPILE AGDA2HS Quantity #-}

Name : Set
Name = String
{-# COMPILE AGDA2HS Name #-}

BName : Set
BName = String
{-# COMPILE AGDA2HS BName #-}


data Term : Set where
  ToCheck : Term  -- terms with a type checkable.
  ToInfer : Term  -- terms that which types can be inferred.

{-# COMPILE AGDA2HS Term #-}

{-
Following [Dunfield and Krishnaswami, 2019], we below follow the
Pfenning principle: "If the rule is an introduction rule, make the
principal judgement "checking", and if the rule is an elimination
rule, make the principal judgement "synthesising." Glossary:

- Principal connective of a rule:
  - for an introduction rule is the connective that is being
    introduced.
  - for a elimination rule is the connective that is eliminated.
- Principal Judgement of a rule is the judgment that contains the
  principal connective.
-}


--------------------------------------------------------------------------------
-- Type-checkable terms.
--------------------------------------------------------------------------------

data CheckableTerm : Set
data InferableTerm : Set

data CheckableTerm where
  -- Universe types.
  UniverseType : CheckableTerm
  -- Dependent function types.
  --   (λ x. t) : Π[ x :ρ S ] P x
  Lam : BName → CheckableTerm → CheckableTerm
  PiType : Quantity → BName → CheckableTerm → CheckableTerm → CheckableTerm
  --   (Π[ x :ρ S ] P x) : U
  -- Dependent tensor product types.
  --   (M , N) : S ⊗ T
  TensorIntro : CheckableTerm → CheckableTerm → CheckableTerm
  --   * S ⊗ T : U
  TensorType : Quantity → BName → CheckableTerm → CheckableTerm → CheckableTerm
  -- Unit types.
  --   ⋆ : 𝟙
  Unit : CheckableTerm
  --   𝟙 : U
  UnitType : CheckableTerm
  -- Disjoint sum types.
  Inl : CheckableTerm → CheckableTerm
  --   inl x : S + T
  Inr : CheckableTerm → CheckableTerm
  --   inr x : S + T
  SumType : CheckableTerm → CheckableTerm → CheckableTerm
  --   S + T : U
  -- Inferrable terms are clearly checkable.
  Inferred : InferableTerm → CheckableTerm

{-# COMPILE AGDA2HS CheckableTerm #-}

data InferableTerm where
  Ann : CheckableTerm → CheckableTerm → InferableTerm
  Bound : Nat → InferableTerm
  Free : Name → InferableTerm
  -- |  eliminator.
  App : InferableTerm → CheckableTerm → InferableTerm
  -- | Dependent Tensor product eliminator.
  TensorTypeElim
    : Quantity -- ^ Multiplicity of the eliminated pair.
    → BName    -- ^ Name of the variable binding the pair in the type
                   -- annotation of the result of elimination.
    → BName -- ^ Name of the variable binding the first element.
    → BName -- ^ Name of the variable binding the second element.
    → InferableTerm -- ^ The eliminated pair.
    → CheckableTerm -- ^ Result of the elimination.
    → CheckableTerm -- ^ Type annotation of the result of elimination.
    → InferableTerm
  -- | Sum type eliminator.
  SumTypeElim -- Case
    :  Quantity -- ^ Multiplicity of the sum contents.
    →  BName -- ^ Name of the variable binding the sum in the type
                  -- annotation of the result of elimination.
    → InferableTerm -- ^ The eliminated sum.
    → BName -- ^ Name of the variable binding the left element.
    → CheckableTerm -- ^ Result of the elimination in case the sum contains the left
            -- element.
    → BName -- ^ Name of the variable binding the right element.
    → CheckableTerm -- ^ Result of the elimination in case the sum contains the right
            -- element.
    → CheckableTerm -- ^ Type annotation of the result of the elimination.
    → InferableTerm
{-# COMPILE AGDA2HS InferableTerm #-}


-- | Substitution on type-synthesising terms.
substInferableTerm
  : Nat  -- ^ bound variable x (Bruijn)
  -> InferableTerm -- inferable term N
  -> InferableTerm -- inferable term M
  -> InferableTerm -- N[x := M]
substInferableTerm v i1 i2 = Bound 0
{-# COMPILE AGDA2HS substInferableTerm #-}

-- | Substitution on type-checkable terms.
checkableSubst
  : Nat  -- ^ bound variable x (Bruijn)
  -> InferableTerm -- inferable term N
  -> CheckableTerm -- checkable term M
  -> CheckableTerm -- N[x := M]
checkableSubst v i c = Unit
{-# COMPILE AGDA2HS checkableSubst #-}

--------------------------------------------------------------------------------
-- Type checking
--------------------------------------------------------------------------------

-- type Resource = Map.Map Name Usage

-- mergeUsages :: (Ord k, Semiring v) => Map.Map k v -> Map.Map k v -> Map.Map k v

-- -- | Synthesise the type of a term and check that context has appropriate
-- -- resources available for the term.
-- synthesise :: (ValueEnv, Context) -> Usage -> ITerm -> Result Type
-- synthesise = P.undefined

-- -- | Synthesise the type of a term.
-- synthType :: Relevance -> ITerm -> TypeJudgment (Usage, Type)
-- synthType = P.undefined

-- -- | Type-check a term.
-- checkType :: Relevance -> CTerm -> Type -> TypeJudgment Usage
-- checkType = P.undefined

-- -- | Type-check a dependent type.
-- checkTypeDep
--   :: Relevance -> BindingName -> CTerm -> CTerm -> CTerm -> TypeJudgment Usage
-- checkTypeDep = P.undefined

-- -- | Type-check an atomic type (a.k.a. types that denote a particular
-- -- set, e.g. 1, Z, N).
-- checkTypeAtom :: Relevance -> CTerm -> TypeJudgment Usage
-- checkTypeAtom = P.undefined

-- checkTypeErased :: CTerm -> Type -> TypeJudgment ()
-- checkTypeErased = P.undefined
