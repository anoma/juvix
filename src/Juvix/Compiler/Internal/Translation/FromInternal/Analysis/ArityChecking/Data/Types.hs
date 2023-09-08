module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types where

import Juvix.Prelude
import Juvix.Prelude.Pretty

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

data ArityRest
  = ArityRestUnit
  | ArityRestUnknown
  deriving stock (Eq)

data UnfoldedArity = UnfoldedArity
  { _ufoldArityParams :: [ArityParameter],
    _ufoldArityRest :: ArityRest
  }
  deriving stock (Eq)

data ArityParameter
  = ParamExplicit Arity
  | ParamImplicit
  | ParamImplicitInstance
  deriving stock (Eq)

makeLenses ''UnfoldedArity

arityParameter :: ArityParameter -> Arity
arityParameter = \case
  ParamImplicit -> ArityUnit
  ParamImplicitInstance -> ArityUnit
  ParamExplicit a -> a

arityCommonPrefix :: Arity -> Arity -> [ArityParameter]
arityCommonPrefix p1 p2 = commonPrefix u1 u2
  where
    u1 = unfoldArity p1
    u2 = unfoldArity p2

unfoldArity' :: Arity -> UnfoldedArity
unfoldArity' = \case
  ArityUnit ->
    UnfoldedArity
      { _ufoldArityParams = [],
        _ufoldArityRest = ArityRestUnit
      }
  ArityUnknown ->
    UnfoldedArity
      { _ufoldArityParams = [],
        _ufoldArityRest = ArityRestUnknown
      }
  ArityFunction (FunctionArity l r) ->
    over ufoldArityParams (l :) (unfoldArity' r)

unfoldArity :: Arity -> [ArityParameter]
unfoldArity = (^. ufoldArityParams) . unfoldArity'

foldArity :: UnfoldedArity -> Arity
foldArity UnfoldedArity {..} = go _ufoldArityParams
  where
    go :: [ArityParameter] -> Arity
    go = \case
      [] -> case _ufoldArityRest of
        ArityRestUnit -> ArityUnit
        ArityRestUnknown -> ArityUnknown
      (a : as) -> ArityFunction (FunctionArity l (go as))
        where
          l = case a of
            ParamExplicit e -> ParamExplicit e
            ParamImplicit -> ParamImplicit
            ParamImplicitInstance -> ParamImplicitInstance

instance HasAtomicity FunctionArity where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Arity where
  atomicity = \case
    ArityUnit -> Atom
    ArityUnknown -> Atom
    ArityFunction f -> atomicity f

instance Pretty ArityParameter where
  pretty = \case
    ParamImplicit -> "{𝟙}"
    ParamImplicitInstance -> "{{𝟙}}"
    ParamExplicit f -> pretty f

instance Pretty FunctionArity where
  pretty f@(FunctionArity l r) =
    parensCond (atomParens (const True) (atomicity f) funFixity) (pretty l)
      <> " → "
      <> pretty r
    where
      parensCond :: Bool -> Doc a -> Doc a
      parensCond t d = if t then parens d else d

instance Pretty Arity where
  pretty = \case
    ArityUnit -> "𝟙"
    ArityUnknown -> "?"
    ArityFunction f -> pretty f
