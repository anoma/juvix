open import Relation.Binary using (Rel)

module Algebra.Structures.StarSemiring
  {a ℓ} {A : Set a}  -- The underlying set
  (_≈_ : Rel A ℓ)    -- The underlying equality relation
  where

open import Base
open import Algebra.Structures {A = A} _≡_


record IsStarSemiring (_+_ _*_ : Op₂ A) (★ : Op₁ A) (0# 1# : A) : Set (a ⊔ ℓ) where
  field
    isSemiring : IsSemiring _+_ _*_ 0# 1#
    ★-cond-1 : ∀ a → ★ a ≈ (1# + (a * ★ a))
    ★-cond-2 : ∀ a → ★ a ≈ (1# + (★ a * a))
