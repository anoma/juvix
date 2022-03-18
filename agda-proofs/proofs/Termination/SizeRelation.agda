module Termination.SizeRelation where

open import Base
open import Relation.Binary.PropositionalEquality.Properties as ≡

data S : Set where
  ⁇ : S
  ≺ : S
  ∼ : S

infixl 6 _+_
infixl 7 _*_

_*_ : Op₂ S
⁇ * _ = ⁇
∼ * a = a
≺ * ⁇ = ⁇
≺ * ∼ = ≺
≺ * ≺ = ≺

_+_ : Op₂ S
≺ + _ = ≺
∼ + ≺ = ≺
∼ + ∼ = ∼
∼ + ⁇ = ∼
⁇ + b = b

module ★1 where
  ★ : Op₁ S
  ★ ⁇ = ∼
  ★ ≺ = ≺
  ★ ∼ = ∼

  private
    ★-condition-1 : (a : S) → ★ a ≡ ∼ + a * ★ a
    ★-condition-1 ⁇ = refl
    ★-condition-1 ≺ = refl
    ★-condition-1 ∼ = refl

    ★-condition-2 : (a : S) → ★ a ≡ ∼ + ★ a * a
    ★-condition-2 ⁇ = refl
    ★-condition-2 ≺ = refl
    ★-condition-2 ∼ = refl

module ★2 where
  ★ : Op₁ S
  ★ ⁇ = ∼
  ★ ≺ = ≺
  ★ ∼ = ≺

  private
    ★-condition-1 : (a : S) → ★ a ≡ ∼ + a * ★ a
    ★-condition-1 ⁇ = refl
    ★-condition-1 ≺ = refl
    ★-condition-1 ∼ = refl

    ★-condition-2 : (a : S) → ★ a ≡ ∼ + ★ a * a
    ★-condition-2 ⁇ = refl
    ★-condition-2 ≺ = refl
    ★-condition-2 ∼ = refl
