{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module MiniJuvix.Syntax.Core where

import Numeric.Natural (Natural)

data Quantity = Zero
              | One
              | Many

data Relevance = Relevant
               | Irrelevant

relevancy :: Quantity -> Relevance
relevancy Zero = Irrelevant
relevancy One = Relevant
relevancy Many = Relevant

type BName = String

type Name = String

data CheckableTerm = UniverseType
                   | PiType Quantity BName CheckableTerm CheckableTerm
                   | Lam BName CheckableTerm
                   | TensorType Quantity BName CheckableTerm CheckableTerm
                   | TensorIntro CheckableTerm CheckableTerm
                   | UnitType
                   | Unit
                   | SumType CheckableTerm CheckableTerm
                   | Inl CheckableTerm
                   | Inr CheckableTerm
                   | Inferred InferableTerm

data InferableTerm = Bound Natural
                   | Free Name
                   | Ann CheckableTerm CheckableTerm
                   | App InferableTerm CheckableTerm
                   | TensorTypeElim Quantity BName BName BName InferableTerm
                                    CheckableTerm CheckableTerm
                   | SumTypeElim Quantity BName InferableTerm BName CheckableTerm
                                 BName CheckableTerm CheckableTerm

checkEq :: CheckableTerm -> CheckableTerm -> Bool
checkEq UniverseType UniverseType = True
checkEq (PiType q₁ _ a₁ b₁) (PiType q₂ _ a₂ b₂)
  = q₁ == q₂ && checkEq a₁ a₂ && checkEq b₁ b₂
checkEq (TensorType q₁ _ a₁ b₁) (TensorType q₂ _ a₂ b₂)
  = q₁ == q₂ && checkEq a₁ a₂ && checkEq b₁ b₂
checkEq (TensorIntro x₁ y₁) (TensorIntro x₂ y₂)
  = checkEq x₁ x₂ && checkEq y₁ y₂
checkEq UnitType UnitType = True
checkEq Unit Unit = True
checkEq (SumType x₁ y₁) (SumType x₂ y₂)
  = checkEq x₁ x₂ && checkEq y₁ y₂
checkEq (Inl x) (Inl y) = checkEq x y
checkEq (Inr x) (Inr y) = checkEq x y
checkEq (Inferred x) (Inferred y) = inferEq x y
checkEq _ _ = False

inferEq :: InferableTerm -> InferableTerm -> Bool
inferEq (Bound x) (Bound y) = x == y
inferEq (Free x) (Free y) = x == y
inferEq (Ann x₁ y₁) (Ann x₂ y₂) = checkEq x₁ x₂ && checkEq y₁ y₂
inferEq (App x₁ y₁) (App x₂ y₂) = inferEq x₁ x₂ && checkEq y₁ y₂
inferEq (TensorTypeElim q₁ _ _ _ a₁ b₁ c₁)
  (TensorTypeElim q₂ _ _ _ a₂ b₂ c₂)
  = q₁ == q₂ && inferEq a₁ a₂ && checkEq b₁ b₂ && checkEq c₁ c₂
inferEq (SumTypeElim q₁ _ x₁ _ a₁ _ b₁ c₁)
  (SumTypeElim q₂ _ x₂ _ a₂ _ b₂ c₂)
  = q₁ == q₂ &&
      inferEq x₁ x₂ && checkEq a₁ a₂ && checkEq b₁ b₂ && checkEq c₁ c₂
inferEq _ _ = False

instance Eq CheckableTerm where
    (==) = checkEq

instance Eq InferableTerm where
    (==) = inferEq

data Term = Checkable CheckableTerm
          | Inferable InferableTerm

termEq :: Term -> Term -> Bool
termEq (Checkable x) (Checkable y) = x == y
termEq (Inferable x) (Inferable y) = x == y
termEq _ _ = False

instance Eq Term where
    (==) = termEq

