{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module MiniJuvix.Syntax.Core where

import Numeric.Natural (Natural)

data Quantity = Zero
              | One
              | Many

data Relevance = Relevant
               | Irrelevant

type Name = String

type BName = String

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

data Term = Checkable CheckableTerm
          | Inferable InferableTerm

