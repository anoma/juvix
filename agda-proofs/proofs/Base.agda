module Base where

open import Level using (Level; _⊔_) renaming (suc to lsuc; zero to lzero) public
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst; trans; sym; cong) public
open import Algebra.Core public
