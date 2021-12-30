module MiniJuvix.Typing.Typechecking () where

--------------------------------------------------------------------------------

import MiniJuvix.Syntax.Core
import MiniJuvix.Syntax.Eval
import MiniJuvix.Typing.Error
import MiniJuvix.Typing.Utils
  ( Binding,
    Quantities,
    TypingContext,
    TypingContextM,
  )
import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------

type TypingResult = Either Error

{- Let's try to have support Andy's notation. Γ |-  (x :q M) -| Δ
   where Γ is the given context, x is a variable of quantity q of type
   M, and Δ is the leftovers of quantities for each variable in Γ.
-}

type LeftOvers = Quantities

type Judgment = TypingContextM TypingResult LeftOvers

-- This should be equivalent to have 0Γ.
type ZeroJudgment = TypingContextM TypingResult ()

extendLocalContext :: String -> Binding -> Judgment -> Judgment
extendLocalContext = undefined

type Type = Value

--------------------------------------------------------------------------------
-- Type checking
--------------------------------------------------------------------------------

check :: Relevance -> Term -> Type -> TypingResult (Type, LeftOvers)
check = undefined

check' :: Relevance -> CheckableTerm -> Type -> TypingResult (Type, LeftOvers)
check' = undefined

--------------------------------------------------------------------------------
-- Type inference
--------------------------------------------------------------------------------

-- | infer the type of a term and check that context has appropriate
-- resources available for the term.
infer :: TypingContext -> Quantity -> Term -> TypingResult (Type, LeftOvers)
infer = undefined

infer' ::
  TypingContext ->
  Quantity ->
  InferableTerm ->
  TypingResult (Type, LeftOvers)
infer' = undefined
