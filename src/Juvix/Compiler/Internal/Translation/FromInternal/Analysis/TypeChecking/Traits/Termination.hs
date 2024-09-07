module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Termination
  ( checkTraitTermination,
    cmpInstanceParam,
  )
where

import Juvix.Compiler.Internal.Extra.InstanceInfo
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.SizeRelation
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew.Options
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude

checkTraitTermination ::
  forall r.
  (Members '[Reader TypeCheckingOptions, Error TypeCheckerError] r) =>
  InstanceApp ->
  InstanceInfo ->
  Sem r ()
checkTraitTermination InstanceApp {..} InstanceInfo {..} = do
  mode <- asks (^. typeCheckingMode)
  case mode of
    TypeCheckingNormal -> mapM_ checkArg _instanceAppArgs
    TypeCheckingBuildCallMap -> return ()
  where
    checkArg :: InstanceParam -> Sem r ()
    checkArg arg =
      unless (any (checkStrictSubterm arg) _instanceInfoParams) $
        throw (ErrTraitNotTerminating (TraitNotTerminating (paramToExpression arg)))

-- | Checks that p1 is a strict subterm of p2
checkStrictSubterm :: InstanceParam -> InstanceParam -> Bool
checkStrictSubterm p1 p2 = case p2 of
  InstanceParamApp InstanceApp {..} ->
    any (checkSubterm p1) _instanceAppArgs
  InstanceParamFun InstanceFun {..} ->
    checkSubterm p1 _instanceFunLeft
      || checkSubterm p1 _instanceFunRight
  _ ->
    False

checkSubterm :: InstanceParam -> InstanceParam -> Bool
checkSubterm p1 p2 = p1 == p2 || checkStrictSubterm p1 p2

cmpInstanceParam :: InstanceParam -> InstanceParam -> Maybe SizeRel'
cmpInstanceParam l r
  | checkStrictSubterm l r = Just RLe
  | l == r = Just REq
  | otherwise = Nothing
