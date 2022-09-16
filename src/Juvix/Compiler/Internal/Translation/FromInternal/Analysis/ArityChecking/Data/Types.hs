module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types where

import Juvix.Prelude

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
  deriving stock (Eq)

makeLenses ''UnfoldedArity

arityParameter :: ArityParameter -> Arity
arityParameter = \case
  ParamImplicit -> ArityUnit
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
