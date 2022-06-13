module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Arity where

import MiniJuvix.Prelude

data Arity
  = ArityUnit
  | ArityFunction FunctionArity
  | ArityUnknown
  deriving stock (Eq)

data FunctionArity = FunctionArity
  { _functionArityLeft :: ArityParameter,
    _functionArityRight :: Arity
  }
  deriving stock (Eq)

data ArityParameter
  = ParamExplicit Arity
  | ParamImplicit
  deriving stock (Eq)

arityParameter :: ArityParameter -> Arity
arityParameter = \case
  ParamImplicit -> ArityUnit
  ParamExplicit a -> a

unfoldArity :: Arity -> [ArityParameter]
unfoldArity = go
  where
    go :: Arity -> [ArityParameter]
    go = \case
      ArityUnit -> []
      ArityUnknown -> []
      ArityFunction (FunctionArity l r) -> l : unfoldArity r

foldArity :: [ArityParameter] -> Arity
foldArity = go
  where
    go = \case
      [] -> ArityUnit
      (a : as) -> ArityFunction (FunctionArity l (go as))
        where
          l = case a of
            ParamExplicit e -> ParamExplicit e
            ParamImplicit -> ParamImplicit
