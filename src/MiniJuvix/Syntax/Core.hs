{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module MiniJuvix.Syntax.Core where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Prelude
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- Quantity (a.k.a. Usage)
--------------------------------------------------------------------------------

data Quantity
  = Zero
  | One
  | Many

instance Eq Quantity where
  Zero == Zero = True
  One == One = True
  Many == Many = True
  _ == _ = False

compareQuantity :: Quantity -> Quantity -> Ordering
compareQuantity Zero Zero = EQ
compareQuantity Zero _ = LT
compareQuantity _ Zero = GT
compareQuantity One One = EQ
compareQuantity One _ = LT
compareQuantity _ One = GT
compareQuantity Many Many = EQ

instance Ord Quantity where
  compare = compareQuantity
  x < y = compareQuantity x y == LT
  x > y = compareQuantity x y == GT
  x <= y = compareQuantity x y /= GT
  x >= y = compareQuantity x y /= LT
  max x y = if compareQuantity x y == LT then y else x
  min x y = if compareQuantity x y == GT then y else x

instance Semigroup Quantity where
  Zero <> _ = Zero
  One <> m = m
  Many <> Zero = Zero
  Many <> One = Many
  Many <> Many = Many

instance Monoid Quantity where
  mempty = Zero

instance Semiring Quantity where
  one = One
  times Zero _ = Zero
  times One m = m
  times Many Zero = Zero
  times Many One = Many
  times Many Many = Many

data Relevance
  = Relevant
  | Irrelevant

deriving stock instance Eq Relevance

deriving stock instance Ord Relevance

relevancy :: Quantity -> Relevance
relevancy Zero = Irrelevant
relevancy _ = Relevant

type Index = Natural

type BindingName = String

data Name
  = Global String
  | Local BindingName Index

instance Eq Name where
  Global x == Global y = x == y
  Local x1 y1 == Local x2 y2 = x1 == x2 && y1 == y2
  _ == _ = False

data Variable
  = Bound Index
  | Free Name

instance Eq Variable where
  Bound x == Bound y = x == y
  Free x == Free y = x == y
  _ == _ = False

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Type-checkable terms.
--------------------------------------------------------------------------------

data CheckableTerm
  = UniverseType
  | PiType Quantity BindingName CheckableTerm CheckableTerm
  | Lam BindingName CheckableTerm
  | TensorType Quantity BindingName CheckableTerm CheckableTerm
  | TensorIntro CheckableTerm CheckableTerm
  | UnitType
  | Unit
  | SumType CheckableTerm CheckableTerm
  | Inl CheckableTerm
  | Inr CheckableTerm
  | Inferred InferableTerm

data InferableTerm
  = Var Variable
  | Ann CheckableTerm CheckableTerm
  | App InferableTerm CheckableTerm
  | TensorTypeElim
      Quantity
      BindingName
      BindingName
      BindingName
      InferableTerm
      CheckableTerm
      CheckableTerm
  | SumTypeElim
      Quantity
      BindingName
      InferableTerm
      BindingName
      CheckableTerm
      BindingName
      CheckableTerm
      CheckableTerm

--------------------------------------------------------------------------------
-- Type-inferable terms (a.k.a terms that synthesise)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Term Equality
--------------------------------------------------------------------------------

checkEq :: CheckableTerm -> CheckableTerm -> Bool
checkEq UniverseType UniverseType = True
checkEq (PiType q₁ _ a₁ b₁) (PiType q₂ _ a₂ b₂) =
  q₁ == q₂ && checkEq a₁ a₂ && checkEq b₁ b₂
checkEq (TensorType q₁ _ a₁ b₁) (TensorType q₂ _ a₂ b₂) =
  q₁ == q₂ && checkEq a₁ a₂ && checkEq b₁ b₂
checkEq (TensorIntro x₁ y₁) (TensorIntro x₂ y₂) =
  checkEq x₁ x₂ && checkEq y₁ y₂
checkEq UnitType UnitType = True
checkEq Unit Unit = True
checkEq (SumType x₁ y₁) (SumType x₂ y₂) =
  checkEq x₁ x₂ && checkEq y₁ y₂
checkEq (Inl x) (Inl y) = checkEq x y
checkEq (Inr x) (Inr y) = checkEq x y
checkEq (Inferred x) (Inferred y) = inferEq x y
checkEq _ _ = False

inferEq :: InferableTerm -> InferableTerm -> Bool
inferEq (Var x) (Var y) = x == y
inferEq (Ann x₁ y₁) (Ann x₂ y₂) = checkEq x₁ x₂ && checkEq y₁ y₂
inferEq (App x₁ y₁) (App x₂ y₂) = inferEq x₁ x₂ && checkEq y₁ y₂
inferEq
  (TensorTypeElim q₁ _ _ _ a₁ b₁ c₁)
  (TensorTypeElim q₂ _ _ _ a₂ b₂ c₂) =
    q₁ == q₂ && inferEq a₁ a₂ && checkEq b₁ b₂ && checkEq c₁ c₂
inferEq
  (SumTypeElim q₁ _ x₁ _ a₁ _ b₁ c₁)
  (SumTypeElim q₂ _ x₂ _ a₂ _ b₂ c₂) =
    q₁ == q₂
      && inferEq x₁ x₂
      && checkEq a₁ a₂
      && checkEq b₁ b₂
      && checkEq c₁ c₂
inferEq _ _ = False

instance Eq CheckableTerm where
  (==) = checkEq

instance Eq InferableTerm where
  (==) = inferEq

data Term
  = Checkable CheckableTerm
  | Inferable InferableTerm

termEq :: Term -> Term -> Bool
termEq (Checkable (Inferred x)) (Inferable y) = x == y
termEq (Checkable x) (Checkable y) = x == y
termEq (Inferable x) (Checkable (Inferred y)) = x == y
termEq (Inferable x) (Inferable y) = x == y
termEq _ _ = False

instance Eq Term where
  (==) = termEq
