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
  | ParamImplicit Arity
  | ParamImplicitInstance
  deriving stock (Eq)

makeLenses ''UnfoldedArity
makeLenses ''FunctionArity

isParamImplicit :: ArityParameter -> Bool
isParamImplicit = \case
  ParamImplicit {} -> True
  _ -> False

hasImplicitness :: IsImplicit -> ArityParameter -> Bool
hasImplicitness i a = case (i, a) of
  (Explicit, ParamExplicit {}) -> True
  (Explicit, _) -> False
  (Implicit, ParamImplicit {}) -> True
  (Implicit, _) -> False
  (ImplicitInstance, ParamImplicitInstance) -> True
  (ImplicitInstance, _) -> False

arityParameter :: ArityParameter -> Arity
arityParameter = \case
  ParamImplicit a -> a
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
      a : as -> ArityFunction (FunctionArity a (go as))

instance HasAtomicity FunctionArity where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Arity where
  atomicity = \case
    ArityUnit -> Atom
    ArityUnknown -> Atom
    ArityFunction f -> atomicity f

instance Pretty ArityParameter where
  pretty = \case
    ParamImplicit i -> "{" <> pretty i <> "}"
    ParamImplicitInstance -> "{{𝟙}}"
    ParamExplicit f -> pretty f

instance HasAtomicity ArityParameter where
  atomicity = \case
    ParamExplicit a -> atomicity a
    ParamImplicit {} -> Atom
    ParamImplicitInstance -> Atom

instance Pretty FunctionArity where
  pretty (FunctionArity l r) =
    ppSide False l
      <> " → "
      <> ppSide True r
    where
      ppSide :: (HasAtomicity a, Pretty a) => Bool -> a -> Doc ann
      ppSide isright lr = parensCond (atomParens (const isright) (atomicity lr) funFixity) (pretty lr)
      parensCond :: Bool -> Doc a -> Doc a
      parensCond t d = if t then parens d else d

instance Pretty Arity where
  pretty = \case
    ArityUnit -> "𝟙"
    ArityUnknown -> "?"
    ArityFunction f -> pretty f

applyArgument :: IsImplicit -> Arity -> Maybe Arity
applyArgument i = \case
  ArityUnknown -> Just ArityUnknown
  ArityUnit -> Nothing
  ArityFunction f
    | hasImplicitness i (f ^. functionArityLeft) -> Just (f ^. functionArityRight)
    | otherwise -> Nothing

-- | If we give identifiers to unknown arities we could do unification and the
-- matching would be more accurate
matchArity :: Arity -> Arity -> Bool
matchArity a1 a2 = isJust . run . runFail $ do
  go a1 a2
  where
    go :: Arity -> Arity -> Sem '[Fail] ()
    go a b = case (a, b) of
      (ArityUnknown {}, _) -> return ()
      (_, ArityUnknown {}) -> return ()
      (ArityUnit, ArityUnit) -> return ()
      (ArityUnit, ArityFunction {}) -> fail
      (ArityFunction {}, ArityUnit) -> fail
      (ArityFunction f1, ArityFunction f2) -> goFunction f1 f2

    goParam :: ArityParameter -> ArityParameter -> Sem '[Fail] ()
    goParam p1 p2 = case (p1, p2) of
      (ParamExplicit a, ParamExplicit b) -> go a b
      (ParamExplicit {}, _) -> fail
      (_, ParamExplicit {}) -> fail
      (ParamImplicit a, ParamImplicit b) -> go a b
      (ParamImplicit {}, _) -> fail
      (_, ParamImplicit {}) -> fail
      (ParamImplicitInstance, ParamImplicitInstance) -> return ()

    goFunction :: FunctionArity -> FunctionArity -> Sem '[Fail] ()
    goFunction f1 f2 = do
      goParam (f1 ^. functionArityLeft) (f2 ^. functionArityLeft)
      go (f1 ^. functionArityRight) (f2 ^. functionArityRight)
