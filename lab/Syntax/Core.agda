{-
This module exposes the Internal syntax. A term is either checkable or
inferable. As the name indicates, a term of type CheckableTerm is a
term we must check. Similarly, a term of type InferableTerm, it is a
term we can infer.
-}

module MiniJuvix.Syntax.Core 
  where

--------------------------------------------------------------------------------

open import Haskell.Prelude
open import Agda.Builtin.Equality

--------------------------------------------------------------------------------
-- Haskell stuff 
--------------------------------------------------------------------------------

{-# FOREIGN AGDA2HS
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#-}

{-# FOREIGN AGDA2HS
--------------------------------------------------------------------------------

import MiniJuvix.Prelude

--------------------------------------------------------------------------------
-- Quantity (a.k.a. Usage)
--------------------------------------------------------------------------------
#-}

data Quantity : Set where
  Zero One Many : Quantity
{-# COMPILE AGDA2HS Quantity #-}

instance
  QuantityEq : Eq Quantity
  QuantityEq ._==_ Zero Zero = true
  QuantityEq ._==_ One  One  = true
  QuantityEq ._==_ Many Many = true
  QuantityEq ._==_ _    _    = false
{-# COMPILE AGDA2HS QuantityEq #-}
 
compareQuantity : Quantity -> Quantity -> Ordering
compareQuantity Zero  Zero = EQ
compareQuantity Zero  _    = LT
compareQuantity _    Zero  = GT
compareQuantity One  One   = EQ
compareQuantity One  _     = LT
compareQuantity _    One   = GT
compareQuantity Many Many  = EQ
{-# COMPILE AGDA2HS compareQuantity #-}

instance
  QuantityOrd : Ord Quantity
  QuantityOrd .compare  = compareQuantity 
  QuantityOrd ._<_  x y = compareQuantity x y == LT
  QuantityOrd ._>_  x y = compareQuantity x y == GT
  QuantityOrd ._<=_ x y = compareQuantity x y /= GT
  QuantityOrd ._>=_ x y = compareQuantity x y /= LT
  QuantityOrd .max  x y = if compareQuantity x y == LT then y else x
  QuantityOrd .min  x y = if compareQuantity x y == GT then y else x
  -- Using ordFromCompare didnn' work, I might need to open an issue
  -- for this in agda2hs, Idk.

-- The type of usages forms an ordered semiring.

instance
  QuantitySemigroup : Semigroup Quantity
  QuantitySemigroup ._<>_ Zero _ = Zero
  QuantitySemigroup ._<>_ One m = m
  QuantitySemigroup ._<>_ Many Zero = Zero
  QuantitySemigroup ._<>_ Many One = Many
  QuantitySemigroup ._<>_ Many Many = Many

  QuantityMon : Monoid Quantity
  QuantityMon .mempty = Zero

  QuantitySemiring : Semiring Quantity
  QuantitySemiring .one = One
  QuantitySemiring .times Zero _ = Zero
  QuantitySemiring .times One m = m
  QuantitySemiring .times Many Zero = Zero
  QuantitySemiring .times Many One = Many
  QuantitySemiring .times Many Many = Many

{-# COMPILE AGDA2HS QuantityOrd #-}
{-# COMPILE AGDA2HS QuantitySemigroup #-}
{-# COMPILE AGDA2HS QuantityMon #-}
{-# COMPILE AGDA2HS QuantitySemiring #-}

--------------------------------------------------------------------------------
-- Being relevant for a term is to have non zero quantity.

data Relevance : Set where
  Relevant   : Relevance  -- terms to compute.
  Irrelevant : Relevance  -- terms to contemplate (for type formation).
{-# COMPILE AGDA2HS Relevance #-}
{-# FOREIGN AGDA2HS
deriving stock instance Eq Relevance
deriving stock instance Ord Relevance
#-}

relevancy : Quantity → Relevance
relevancy Zero = Irrelevant
relevancy _    = Relevant
{-# COMPILE AGDA2HS relevancy #-}

--------------------------------------------------------------------------------
-- Variables. Relevant on the following design is the separation for a
-- variable between Bound and Free as a data constructr, due to
-- McBride and McKinna in "Functional Pearl: I am not a Number—I am a
-- Free Variable". 
--------------------------------------------------------------------------------

-- DeBruijn index.
Index : Set
Index = Nat
{-# COMPILE AGDA2HS Index #-}

-- A variable can be "bound", "binding bound", or simply free. For
-- example, consider  the term "x(λy.y)". The variable x is free, the
-- first y is a binding bound variable, and the latter y is bound.

BindingName : Set
BindingName = String
{-# COMPILE AGDA2HS BindingName #-}

-- A named variable can be in a local or global enviroment. In the
-- lambda term above, for example, the second occurrence of y is
-- globally bound. However, inside the the body of the lambda, the
-- variable is local free.

data Name : Set where
  -- the variable has zero binding 
  Global : String → Name
  -- the variable has a binding in its scope.
  Local : BindingName → Index → Name
{-# COMPILE AGDA2HS Name #-}

instance
  nameEq : Eq Name
  nameEq ._==_ (Global x) (Global y) = x == y
  nameEq ._==_ (Local x1 y1) (Local x2 y2) = x1 == x2 && y1 == y2
  nameEq ._==_ _ _ = false
{-# COMPILE AGDA2HS nameEq #-}

-- A variable is then a number indicating its DeBruijn index.
-- Otherwise, it is free, with an identifier as a name, or
-- inside 
data Variable : Set where
  Bound : Index → Variable
  Free  : Name → Variable
{-# COMPILE AGDA2HS Variable #-}

instance
  variableEq : Eq Variable 
  variableEq ._==_ (Bound x) (Bound y) = x == y
  variableEq ._==_ (Free x) (Free y) = x == y
  variableEq ._==_ _ _ = false
{-# COMPILE AGDA2HS variableEq #-}

-- TODO: May I want to have Instances of Ord, Functor, Applicative, Monad?

{-# FOREIGN AGDA2HS
--------------------------------------------------------------------------------
#-}

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

{-# FOREIGN AGDA2HS
--------------------------------------------------------------------------------
-- Type-checkable terms.
--------------------------------------------------------------------------------
#-}

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
  PiType : Quantity → BindingName → CheckableTerm → CheckableTerm → CheckableTerm
  Lam : BindingName → CheckableTerm → CheckableTerm
  {- Dependent tensor product types. 
  See the typing rules ⊗-F-⇐,  ⊗-I₀⇐, and ⊗-I₁⇐.
    1. * S ⊗ T : U
    2. (M , N) : S ⊗ T
  -}
  TensorType : Quantity → BindingName → CheckableTerm → CheckableTerm → CheckableTerm
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

{-# FOREIGN AGDA2HS
--------------------------------------------------------------------------------
-- Type-inferable terms (a.k.a terms that synthesise)
--------------------------------------------------------------------------------
#-}

data InferableTerm where
  -- | Variables, typing rule Var⇒. 
  Var : Variable → InferableTerm
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
    → BindingName          -- z is the name of the variable binding the pair in the
                     -- type annotation of the result of elimination.
    → BindingName          -- u is the name of the variable binding the first element.
    → BindingName          -- v is the name of the variable binding the second element.
    → InferableTerm  -- (u,v) is the eliminated pair.
    → CheckableTerm  -- Result of the elimination.
    → CheckableTerm  -- Type annotation of the result of elimination.
    → InferableTerm
  -- | Sum type eliminator (a.k.a. case)
  -- let (z : S + T) in (case z of {(inl u) ↦ r1; (inr v) ↦ r2}  :^q  T) 
  SumTypeElim        -- Case
    :  Quantity      -- Multiplicity of the sum contents.
    →  BindingName         -- Name of the variable binding the sum in the type
                     -- annotation of the result of elimination.
    → InferableTerm  -- The eliminated sum.
    → BindingName          -- u is the name of the variable binding the left element.
    → CheckableTerm  -- r1 is the result of the elimination in case the sum contains
                     -- the left element.
    → BindingName          -- v is the name of the variable binding the right element.
    → CheckableTerm  -- r2 is the result of the elimination in case the sum contains
                     -- the right element.
    → CheckableTerm  -- Type annotation of the result of the elimination.
    → InferableTerm
{-# COMPILE AGDA2HS InferableTerm #-}

{-# FOREIGN AGDA2HS
--------------------------------------------------------------------------------
-- Term Equality
--------------------------------------------------------------------------------
#-}

checkEq : CheckableTerm → CheckableTerm → Bool
inferEq : InferableTerm → InferableTerm → Bool

-- We below explicitly use `checkEq` and `inferEq` to have a more
-- reliable Haskell output using Agda2HS. The traditional way gives an
-- extraneous instance definitions.

checkEq UniverseType UniverseType = true
checkEq (PiType q₁ _ a₁ b₁) (PiType q₂ _ a₂ b₂) 
  = q₁ == q₂ && checkEq a₁ a₂ && checkEq b₁ b₂
checkEq (TensorType q₁ _ a₁ b₁) (TensorType q₂ _ a₂ b₂)
   = q₁ == q₂ && checkEq a₁ a₂ &&  checkEq b₁ b₂
checkEq (TensorIntro x₁ y₁) (TensorIntro x₂ y₂) = checkEq x₁ x₂ && checkEq y₁ y₂
checkEq UnitType UnitType = true
checkEq Unit Unit = true
checkEq (SumType x₁ y₁) (SumType x₂ y₂) = checkEq x₁ x₂ && checkEq y₁ y₂
checkEq (Inl x) (Inl y) = checkEq  x y
checkEq (Inr x) (Inr y) = checkEq  x y
checkEq (Inferred x) (Inferred y) = inferEq x y
checkEq _ _ = false
{-# COMPILE AGDA2HS checkEq #-}

inferEq (Var x) (Var y) = x == y
inferEq (Ann x₁ y₁) (Ann x₂ y₂) = checkEq x₁ x₂ &&  checkEq y₁ y₂
inferEq (App x₁ y₁) (App x₂ y₂) =  inferEq x₁ x₂ &&  checkEq y₁ y₂
inferEq (TensorTypeElim q₁ _ _ _ a₁ b₁ c₁) (TensorTypeElim q₂ _ _ _ a₂ b₂ c₂) 
  = q₁ == q₂ &&  inferEq a₁ a₂ &&  checkEq b₁ b₂ &&  checkEq c₁ c₂
inferEq (SumTypeElim q₁ _ x₁ _ a₁ _ b₁ c₁) 
      (SumTypeElim q₂ _ x₂ _ a₂ _ b₂ c₂)
  = q₁ == q₂ &&  inferEq x₁ x₂ && checkEq a₁ a₂ &&  checkEq b₁ b₂ &&  checkEq c₁ c₂
inferEq _ _ = false
{-# COMPILE AGDA2HS inferEq #-}

instance
  CheckableTermEq : Eq CheckableTerm
  CheckableTermEq ._==_ = checkEq
{-# COMPILE AGDA2HS CheckableTermEq #-}

instance
  InferableTermEq : Eq InferableTerm
  InferableTermEq ._==_ = inferEq
{-# COMPILE AGDA2HS InferableTermEq #-}

data Term : Set where
  Checkable : CheckableTerm → Term  -- terms with a type checkable.
  Inferable : InferableTerm → Term  -- terms that which types can be inferred.
{-# COMPILE AGDA2HS Term #-}

termEq : Term → Term → Bool
termEq (Checkable (Inferred x))  (Inferable y) = x == y
termEq (Checkable x) (Checkable y) = x == y
termEq (Inferable x) (Checkable (Inferred y)) = x == y
termEq (Inferable x) (Inferable y) = x == y
termEq _ _ = false
{-# COMPILE AGDA2HS termEq #-}

instance 
  TermEq : Eq Term 
  TermEq ._==_ = termEq

{-# COMPILE AGDA2HS TermEq #-}   

--------------------------------------------------------------------------------
-- Other Instances
--------------------------------------------------------------------------------