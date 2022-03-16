-- testing
module MiniJuvix.Syntax.Core where

open import Haskell.Prelude
open import Agda.Builtin.Equality

-- language extensions
{-# FOREIGN AGDA2HS
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
#-}

{-# FOREIGN AGDA2HS
{-
M , N := x
    | λ x . M
    | M N
    | ⊤
    | ⊥
    | if_then_else
    | x : M
where
    variables x.
    M := Bool | M -> M
-}
#-}

VarType : Set
VarType = String

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Type : Set where
  BoolType : Type
  ArrowType : Type → Type → Type

{-# COMPILE AGDA2HS Type deriving (Show, Eq) #-}

-------------------------------------------------------------------------------
-- Terms
-------------------------------------------------------------------------------

data Term : Set where
  Var : VarType → Term
  TT : Term
  FF : Term
  Abs : VarType → Term → Term
  App : Term → Term → Term
  If : Term → Term → Term → Term
  Jud : Term → Type → Term


{-# COMPILE AGDA2HS Term deriving (Show, Eq) #-}

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

Context : Set
Context = List (VarType × Type)

{-# COMPILE AGDA2HS Context #-}

-------------------------------------------------------------------------------

-- | Bidirectional type-checking algorithm:
-- defined by mutual recursion:

-- type inference (a.k.a. type synthesis).
infer : Context → Term → Maybe Type
-- type checking.
check : Context → Term → Type → Maybe Type


codomain : Type → Maybe Type
codomain BoolType = Nothing
codomain (ArrowType a b) = Just b

helper : Context → Term →  Maybe Type → Maybe Type
helper γ x (Just (ArrowType _ tB)) = check γ x tB
helper _ _ _ = Nothing

{-# COMPILE AGDA2HS helper #-}

-- http://cdwifi.cz/#/

infer γ (Var x) = lookup x γ
infer γ TT = pure BoolType
infer γ FF = pure BoolType
infer γ (Abs x t) = pure BoolType -- TODO
infer γ (App f x) = case (infer γ f) of helper γ x
infer γ (If a t f) = pure BoolType
infer γ (Jud x m) = check γ x m

check γ x T = {!!}

{-# COMPILE AGDA2HS infer #-}
{-# COMPILE AGDA2HS check #-}
