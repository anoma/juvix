module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types where

import Juvix.Compiler.Internal.Language
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

data ArityParameter = ArityParameter
  { _arityParameterArity :: Arity,
    _arityParameterImplicit :: IsImplicit,
    _arityParameterInfo :: ArgInfo
  }

instance Eq ArityParameter where
  (ArityParameter ari impl _info) == (ArityParameter ari' impl' _info') =
    (ari, impl) == (ari', impl')

makeLenses ''UnfoldedArity
makeLenses ''ArityParameter

arityParameterName :: Lens' ArityParameter (Maybe Name)
arityParameterName = arityParameterInfo . argInfoName

unfoldingArity :: (UnfoldedArity -> UnfoldedArity) -> Arity -> Arity
unfoldingArity f = foldArity . f . unfoldArity'

arityParameter :: ArityParameter -> Arity
arityParameter = (^. arityParameterArity)

arityParameterImplicitOrInstance :: ArityParameter -> Bool
arityParameterImplicitOrInstance a = case a ^. arityParameterImplicit of
  Explicit {} -> False
  Implicit {} -> True
  ImplicitInstance -> True

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
      (a : as) -> ArityFunction (FunctionArity a (go as))

instance HasAtomicity FunctionArity where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Arity where
  atomicity = \case
    ArityUnit -> Atom
    ArityUnknown -> Atom
    ArityFunction f -> atomicity f

instance Pretty ArityParameter where
  pretty a =
    let ari = arityParameter a
     in case a ^. arityParameterImplicit of
          Implicit -> "{" <> pretty ari <> "}"
          ImplicitInstance -> "{{𝟙}}"
          Explicit -> pretty ari

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
