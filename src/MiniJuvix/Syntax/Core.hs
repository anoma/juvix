{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module MiniJuvix.Syntax.Core where

import Numeric.Natural (Natural)

data Quantity = Zero
              | One
              | Many

type Name = String

type BName = String

data Term = ToCheck
          | ToInfer

data CheckableTerm = UniverseType
                   | Lam BName CheckableTerm
                   | PiType Quantity BName CheckableTerm CheckableTerm
                   | TensorIntro CheckableTerm CheckableTerm
                   | TensorType Quantity BName CheckableTerm CheckableTerm
                   | Unit
                   | UnitType
                   | Inl CheckableTerm
                   | Inr CheckableTerm
                   | SumType CheckableTerm CheckableTerm
                   | Inferred InferableTerm

data InferableTerm = Ann CheckableTerm CheckableTerm
                   | Bound Natural
                   | Free Name
                   | App InferableTerm CheckableTerm
                   | TensorTypeElim Quantity BName BName BName InferableTerm
                                    CheckableTerm CheckableTerm
                   | SumTypeElim Quantity BName InferableTerm BName CheckableTerm
                                 BName CheckableTerm CheckableTerm

substInferableTerm ::
                   Natural -> InferableTerm -> InferableTerm -> InferableTerm
substInferableTerm v i1 i2 = Bound 0

checkableSubst ::
               Natural -> InferableTerm -> CheckableTerm -> CheckableTerm
checkableSubst v i c = Unit

