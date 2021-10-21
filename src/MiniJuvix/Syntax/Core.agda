{-
This module exposes the Internal syntax. A term is either checkable or
inferable. As the name indicates, a term of type CheckableTerm is a
term we must check. Similarly, a term of type InferableTerm, it is a
term we can infer.
-}
module MiniJuvix.Syntax.Core where

--------------------------------------------------------------------------------

open import Haskell.Prelude
open import Agda.Builtin.Equality

-- Haskell language extensions
{-# FOREIGN AGDA2HS
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
#-}

--------------------------------------------------------------------------------

data Quantity : Set where
  Zero One Many : Quantity
{-# COMPILE AGDA2HS Quantity #-}

--------------------------------------------------------------------------------
-- Being relevant for a term is to have non zero quantity.
data Relevance : Set where
  Relevant   : Relevance  -- terms to compute.
  Irrelevant : Relevance  -- terms to contemplate (for type formation).
{-# COMPILE AGDA2HS Relevance #-}

relevancy : Quantity → Relevance
relevancy Zero = Irrelevant
relevancy One  = Relevant
relevancy Many = Relevant

--------------------------------------------------------------------------------

Name : Set
Name = String
{-# COMPILE AGDA2HS Name #-}

BName : Set
BName = String
{-# COMPILE AGDA2HS BName #-}

--------------------------------------------------------------------------------


{- 
Core syntax follows the pattern design for bidirectional typing
algorithmgs in [Dunfield and Krishnaswami, 2019]. Pfenning's principle
is one of such criterion and stated as follows.

1. If the rule is an introduction rule, make the principal judgement
   "checking", and 
2. if the rule is an elimination rule, make the principal judgement
   "synthesising".
   
Jargon:
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
  {- Universe types. 
  See the typing rule Univ⇐.
  -}
  UniverseType : CheckableTerm
  {- Dependent function types. 
  See the typing rules →F⇐ and →I⇐.
    1. (Π[ x :ρ S ] P x) : U
    2. (λ x. t) : Π[ x :ρ S ] P x
  -}
  PiType : Quantity → BName → CheckableTerm → CheckableTerm → CheckableTerm
  Lam : BName → CheckableTerm → CheckableTerm
  {- Dependent tensor product types. 
  See the typing rules ⊗-F-⇐,  ⊗-I₀⇐, and ⊗-I₁⇐.
    1. * S ⊗ T : U
    2. (M , N) : S ⊗ T
  -}
  TensorType : Quantity → BName → CheckableTerm → CheckableTerm → CheckableTerm
  TensorIntro : CheckableTerm → CheckableTerm → CheckableTerm
  {- Unit types. 
  See the typing rule 1-F-⇐ and 1-I-⇐.
    1. 𝟙 : U
    2. ⋆ : 𝟙
  -}
  UnitType : CheckableTerm
  Unit : CheckableTerm
  {- Disjoint sum types.
  See the typing rules
    1. S + T : U
    2. inl x : S + T
    3. inr x : S + T
  -}
  SumType : CheckableTerm → CheckableTerm → CheckableTerm
  Inl : CheckableTerm → CheckableTerm
  Inr : CheckableTerm → CheckableTerm
  -- Inferrable terms are clearly checkable, see typing rule Inf⇐.
  Inferred : InferableTerm → CheckableTerm

{-# COMPILE AGDA2HS CheckableTerm #-}

--------------------------------------------------------------------------------
-- Type-inferable terms (a.k.a terms that synthesise)
--------------------------------------------------------------------------------

data InferableTerm where
  -- | Variables, typing rule Var⇒.
  Bound : Nat → InferableTerm
  Free : Name → InferableTerm
  -- | Annotations, typing rule Ann⇒.
  {- Maybe, I want to have the rules here like this:
  
    OΓ ⊢ S ⇐0 𝕌     Γ ⊢ M ⇐0 𝕌
    ­────────────────────────────── Ann⇒
           Γ ⊢ (M : S) ⇒ S
  -}
  Ann : CheckableTerm → CheckableTerm → InferableTerm
  -- |  Application (eliminator).
  App : InferableTerm → CheckableTerm → InferableTerm
  -- | Dependent Tensor product eliminator. See section 2.1.3 in Atkey 2018.
  -- let z@(u, v) = M in N :^q (a ⊗ b))
  TensorTypeElim
    : Quantity       -- q is the multiplicity of the eliminated pair.
    → BName          -- z is the name of the variable binding the pair in the
                     -- type annotation of the result of elimination.
    → BName          -- u is the name of the variable binding the first element.
    → BName          -- v is the name of the variable binding the second element.
    → InferableTerm  -- (u,v) is the eliminated pair.
    → CheckableTerm  -- Result of the elimination.
    → CheckableTerm  -- Type annotation of the result of elimination.
    → InferableTerm
  -- | Sum type eliminator (a.k.a. case)
  -- let (z : S + T) in (case z of {(inl u) ↦ r1; (inr v) ↦ r2}  :^q  T) 
  SumTypeElim        -- Case
    :  Quantity      -- Multiplicity of the sum contents.
    →  BName         -- Name of the variable binding the sum in the type
                     -- annotation of the result of elimination.
    → InferableTerm  -- The eliminated sum.
    → BName          -- u is the name of the variable binding the left element.
    → CheckableTerm  -- r1 is the result of the elimination in case the sum contains
                     -- the left element.
    → BName          -- v is the name of the variable binding the right element.
    → CheckableTerm  -- r2 is the result of the elimination in case the sum contains
                     -- the right element.
    → CheckableTerm  -- Type annotation of the result of the elimination.
    → InferableTerm
{-# COMPILE AGDA2HS InferableTerm #-}


data Term : Set where
  Checkable : CheckableTerm → Term  -- terms with a type checkable.
  Inferable : InferableTerm → Term  -- terms that which types can be inferred.
  
{-# COMPILE AGDA2HS Term #-}