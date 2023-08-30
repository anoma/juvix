module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Resolver where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Extra
import Juvix.Prelude

resolveTraitInstance ::
  Members '[Error TraitError, NameIdGen, Reader LocalVars] r =>
  InstanceTable ->
  Expression ->
  Sem r Expression
resolveTraitInstance tab ty = do
  vars <- ask
  let tab' = foldr (flip updateInstanceTable) tab (varsToInstances vars)
  is <- lookupInstance tab' ty
  case is of
    [(ii, subs)] -> expandArity (subsIToE subs) (ii ^. instanceInfoArgs) (ii ^. instanceInfoResult)
    [] -> throw (ErrNoInstance (NoInstance ty))
    _ -> throw (ErrAmbiguousInstances (AmbiguousInstances ty (map fst is)))

varsToInstances :: LocalVars -> [InstanceInfo]
varsToInstances LocalVars {..} =
  mapMaybe
    (instanceFromTypedExpression False . mkTyped)
    (HashMap.toList _localTypes)
  where
    mkTyped :: (VarName, Expression) -> TypedExpression
    mkTyped (v, ty) =
      TypedExpression
        { _typedType = ty,
          _typedExpression = ExpressionIden (IdenVar v)
        }

expandArity ::
  Members '[Error TraitError, NameIdGen] r =>
  SubsE ->
  [FunctionParameter] ->
  Expression ->
  Sem r Expression
expandArity subs params e = case params of
  [] ->
    return e
  FunctionParameter {..} : params'
    | Just (Just t) <- flip HashMap.lookup subs <$> _paramName ->
        expandArity subs params' (ExpressionApplication (Application e t _paramImplicit))
    | _paramImplicit == Implicit -> do
        h <- newHole (getLoc e)
        expandArity subs params' (ExpressionApplication (Application e (ExpressionHole h) Implicit))
    | _paramImplicit == ImplicitInstance -> do
        h <- newHole (getLoc e)
        expandArity subs params' (ExpressionApplication (Application e (ExpressionInstanceHole h) ImplicitInstance))
    | otherwise ->
        throw (ErrExplicitInstanceArgument (ExplicitInstanceArgument e))

newHole :: (Member NameIdGen r) => Interval -> Sem r Hole
newHole loc = mkHole loc <$> freshNameId
