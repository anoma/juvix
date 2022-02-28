module Termination.FunctionCall where

open import Base
open import Relation.Binary.PropositionalEquality.Properties as ≡
open import Data.Product.Properties
open import Data.Product
import Termination.SizeRelation as S
open S using (S)
import Termination.SizeRelation.Properties as S
open import Data.Nat using (ℕ)
open import Axiom.Extensionality.Propositional

module Matrix where
  open import Data.Fin using (Fin; zero; suc; inject₁; fromℕ; _≟_)
  open import Data.Nat using (ℕ)
  open import Data.Vec using (Vec; tabulate)
  open import Function.Base using (case_of_)
  open import Relation.Nullary

  -- Square matrix
  Matrix : (A : Set) → ℕ → Set
  Matrix A n = Vec (Vec A n) n

  diagonal : {A : Set} → (zero diag : A) → (n : ℕ) → Matrix A n
  diagonal z diag n = tabulate (λ i → tabulate (λ j →
    case i ≟ j of λ {(yes _) → diag; (no _) → z}))


module Square (n : ℕ) where
  open import Data.Fin using (Fin; zero; suc; inject₁; fromℕ)
  open import Data.Vec
  open Matrix

  -- All calls are assumed to be of arity n
  Call : Set
  Call = Vec S n

  -- All edges are assumed to have n calls
  Edge : Set
  Edge = Matrix S n

  Call-⁇ : Call
  Call-⁇ = replicate S.⁇

  -- Call-∼ : Call
  -- Call-∼ = replicate S.∼

  ⁇ : Edge
  ⁇ = replicate (replicate S.⁇)

  ∼ : Edge
  ∼ = diagonal S.⁇ S.∼ n

  infixl 6 _+_
  infixl 7 _*_

  sumRow : Call → S
  sumRow = foldr _ S._+_ S.⁇

  _*_ : Op₂ Edge
  _*_ a b = tabulate (λ i → tabulate (λ j → sumRow (zipWith S._*_ (lookup a i) (lookup (transpose b) j))))

  -- pointwise
  _+_ : Op₂ Edge
  (a + b) = tabulate (λ i → tabulate (λ j → lookup (lookup a i) j S.+ lookup (lookup b i) j))

  -- ★ : Op₁ Edge
  -- ★ a = tabulate (λ i → tabulate (λ j → S.★ (lookup (lookup a i) j)))


module 2by2 where
  open Square 2
  open import Data.Vec
  open S.★1 renaming (★ to S★)

  ★ : Op₁ Edge
  ★ ((b ∷ c ∷ []) ∷ (d ∷ e ∷ []) ∷ []) =
    let ★b = S★ b
        Δ = e S.+ (d S.* ★b S.* c)
        ★Δ = S★ Δ

        b' = ★b S.+ ★b S.* c S.* ★Δ S.* d S.* ★b
        b'' = S★ (b S.+ c S.* S★ e S.* d)
        c' = ★b S.* c S.* ★Δ
        d' = ★Δ S.* d S.* ★b
        e' = ★Δ
    in ((b'' ∷ c' ∷ []) ∷
        (d' ∷ e' ∷ [])
        ∷ [])

  -- TODO See definition of Call matrix!
  ★-condition-1 : (a : Edge) → ★ a ≡ ∼ + a * ★ a
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.⁇ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.≺ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.⁇ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.≺ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.⁇ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.≺ ∷ S.∼ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.⁇ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.≺ ∷ []) ∷ []) = refl
  ★-condition-1 ((S.∼ ∷ S.∼ ∷ []) ∷ (S.∼ ∷ S.∼ ∷ []) ∷ []) = refl



-- Simplified version
module SingleCall where
  open S.★1 renaming (★ to S★)

  -- All calls have exactly 2 arguments
  Call₂ : Set
  Call₂ = S × S

  -- An edge is a single function call
  Edge₁ : Set
  Edge₁ = Call₂

  ⁇ : Edge₁
  ⁇ = S.⁇ , S.⁇

  ∼ : Edge₁
  ∼ = S.∼ , S.∼

  infixl 6 _+_
  infixl 7 _*_

  _*_ : Op₂ Edge₁
  _*_ (a , b) (a' , b') = a S.* a' , b S.* b'

  _+_ : Op₂ Edge₁
  (a , b) + (a' , b') = a S.+ a' , b S.+ b'

  open import Algebra.Structures {A = Call₂} _≡_
  open import Algebra.Definitions {A = Call₂} _≡_
  open import Algebra.Structures.StarSemiring {A = Call₂} _≡_

  ×-≡,≡→≡ : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} → {p₁@(a₁ , b₁) p₂@(a₂ , b₂) : A × B} → (a₁ ≡ a₂ × b₁ ≡ b₂) → p₁ ≡ p₂
  ×-≡,≡→≡ (refl , refl) = refl

  +-Commutative : Commutative _+_
  +-Commutative a b = ×-≡,≡→≡
    (S.+-Commutative (proj₁ a) (proj₁ b)
    , S.+-Commutative (proj₂ a) (proj₂ b))

  +-Associative : Associative _+_
  +-Associative a b c = ×-≡,≡→≡
     (S.+-Associative (proj₁ a) _ _
     , S.+-Associative (proj₂ a) _ _)

  +-IsMagma : IsMagma _+_
  +-IsMagma = record
    { isEquivalence = ≡.isEquivalence ;
    ∙-cong = λ { refl refl → refl }}

  +-IsSemigroup : IsSemigroup _+_
  +-IsSemigroup = record { isMagma = +-IsMagma ; assoc = +-Associative }

  +-Identityˡ : LeftIdentity ⁇ _+_
  +-Identityˡ _ = refl

  +-Identityʳ : RightIdentity ⁇ _+_
  +-Identityʳ (fst , snd) = ×-≡,≡→≡ (S.+-Identityʳ _ , S.+-Identityʳ _)

  +-Identity : Identity ⁇ _+_
  +-Identity = +-Identityˡ , +-Identityʳ

  +-IsMonoid : IsMonoid _+_ ⁇
  +-IsMonoid = record { isSemigroup = +-IsSemigroup ; identity = +-Identity }

  +-IsCommutativeMonoid : IsCommutativeMonoid _+_ ⁇
  +-IsCommutativeMonoid = record
    { isMonoid = +-IsMonoid ;
    comm = +-Commutative }

  -- Proofs on _*_

  *-Associative : Associative _*_
  *-Associative a b c = ×-≡,≡→≡
     (S.*-Associative (proj₁ a) _ _
     , S.*-Associative (proj₂ a) _ _)

  *-Identityˡ : LeftIdentity ∼ _*_
  *-Identityˡ _ = refl

  *-Identityʳ : RightIdentity ∼ _*_
  *-Identityʳ (fst , snd) = ×-≡,≡→≡ (S.*-Identityʳ _ , S.*-Identityʳ _)

  *-Identity : Identity ∼ _*_
  *-Identity = *-Identityˡ , *-Identityʳ

  -- Proofs on + and *

  *-DistributesOverˡ-+ : _*_ DistributesOverˡ _+_
  *-DistributesOverˡ-+ a b c = ×-≡,≡→≡
   (S.*-DistributesOverˡ-+ (proj₁ a) _ _
   , S.*-DistributesOverˡ-+ (proj₂ a) _ _)

  *-DistributesOverʳ-+ : _*_ DistributesOverʳ _+_
  *-DistributesOverʳ-+ a b c = ×-≡,≡→≡
   (S.*-DistributesOverʳ-+ (proj₁ a) (proj₁ b) _
   , S.*-DistributesOverʳ-+ (proj₂ a) (proj₂ b) _)

  *-DistributesOver-+ : _*_ DistributesOver _+_
  *-DistributesOver-+ = *-DistributesOverˡ-+ , *-DistributesOverʳ-+

  *-IsMagma : IsMagma _*_
  *-IsMagma = record
   { isEquivalence = ≡.isEquivalence
    ; ∙-cong = λ {refl refl → refl }}

  *-IsSemigroup : IsSemigroup _*_
  *-IsSemigroup = record { isMagma = *-IsMagma ; assoc = *-Associative }

  *-IsMonoid : IsMonoid _*_ ∼
  *-IsMonoid = record { isSemigroup = *-IsSemigroup ; identity = *-Identity }

  +-*-IsSemiringWithoutAnnihilatingZero : IsSemiringWithoutAnnihilatingZero _+_ _*_ ⁇ ∼
  +-*-IsSemiringWithoutAnnihilatingZero = record
    { +-isCommutativeMonoid = +-IsCommutativeMonoid
    ; *-isMonoid = *-IsMonoid
    ; distrib = *-DistributesOver-+
    }

  *-LeftZero : LeftZero ⁇ _*_
  *-LeftZero _ = refl

  *-RightZero : RightZero ⁇ _*_
  *-RightZero x = ×-≡,≡→≡
    (S.*-RightZero (proj₁ x)
    , S.*-RightZero (proj₂ x))

  *-Zero : Zero ⁇ _*_
  *-Zero = *-LeftZero , *-RightZero

  +-*-IsSemiring : IsSemiring _+_ _*_ ⁇ ∼
  +-*-IsSemiring = record
    { isSemiringWithoutAnnihilatingZero = +-*-IsSemiringWithoutAnnihilatingZero
    ; zero = *-Zero }

  ★ : Op₁ Edge₁
  ★ (a , b) = S★ a , S★ b

  ★-condition-1 : (a : Edge₁) → ★ a ≡ ∼ + a * ★ a
  ★-condition-1 a = ×-≡,≡→≡
    (S.★-condition-1 (proj₁ a)
    , S.★-condition-1 (proj₂ a))

  ★-condition-2 : (a : Edge₁) → ★ a ≡ ∼ + ★ a * a
  ★-condition-2 a = ×-≡,≡→≡
    (S.★-condition-2 (proj₁ a)
    , S.★-condition-2 (proj₂ a))

  +-*-★-IsStarSemiring : IsStarSemiring _+_ _*_ ★ ⁇ ∼
  +-*-★-IsStarSemiring = record
    { isSemiring = +-*-IsSemiring
    ; ★-cond-1 = ★-condition-1
    ; ★-cond-2 = ★-condition-2 }
