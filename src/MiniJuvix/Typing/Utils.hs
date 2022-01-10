module MiniJuvix.Typing.Utils
  ( Binding (..),
    Context,
    TypingContextM,
    weakenGlobal,
    weakenLocal,
    Quantities,
    checkResources,
    TypingContext (..),
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Syntax.Core (Name, Quantity)
import MiniJuvix.Syntax.Eval (Value)
import MiniJuvix.Utils.Prelude
import qualified MiniJuvix.Utils.Prelude as Map

--------------------------------------------------------------------------------

{-
  Γ  ⊢ let Δ in (x :^q M)

  On the left side of the turnstile, we have the global context (Γ).
  On the right side of the turnstile, we have the local context (Δ). A
  context, regardless its kind, consists of triples, each consisting
  of a variable, its quantity, and its type.
-}

data Binding = Binding
  { varName :: Name,
    varQuantity :: Quantity,
    varType :: Value
  }

type Context = [Binding]

data TypingContext = TypingContext
  { globalEnv :: Context,
    localEnv :: Context
  }

type TypingContextM = ReaderT TypingContext

weakenGlobal :: Binding -> TypingContext -> TypingContext
weakenGlobal var ctxt = ctxt {globalEnv = var : globalEnv ctxt}

weakenLocal :: Binding -> TypingContext -> TypingContext
weakenLocal var ctxt = ctxt {localEnv = var : localEnv ctxt}

-- a.k.a. Resources
type Quantities = Map.Map Name Quantity

checkResources ::
  Context ->
  Quantity ->
  Maybe [(Binding, Quantity)]
checkResources = undefined

mergeResources :: Quantities -> Quantities -> Quantities
mergeResources = undefined
