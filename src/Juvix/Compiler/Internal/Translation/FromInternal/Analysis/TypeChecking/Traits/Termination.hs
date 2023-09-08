module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Termination
  ( checkTraitTermination,
  )
where

import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude

checkTraitTermination ::
  forall r.
  (Member (Error TypeCheckerError) r) =>
  InstanceApp ->
  InstanceInfo ->
  Sem r ()
checkTraitTermination InstanceApp {..} InstanceInfo {..} =
  mapM_ checkArg _instanceAppArgs
  where
    checkArg :: InstanceParam -> Sem r ()
    checkArg arg
      | any (checkStrictSubterm arg) _instanceInfoParams =
          return ()
      | otherwise =
          throw (ErrTraitNotTerminating (TraitNotTerminating (paramToExpression arg)))

checkStrictSubterm :: InstanceParam -> InstanceParam -> Bool
checkStrictSubterm p1 p2 = case p2 of
  InstanceParamApp InstanceApp {..} ->
    any (checkSubterm p1) _instanceAppArgs
  _ ->
    False

checkSubterm :: InstanceParam -> InstanceParam -> Bool
checkSubterm p1 p2 = p1 == p2 || checkStrictSubterm p1 p2
