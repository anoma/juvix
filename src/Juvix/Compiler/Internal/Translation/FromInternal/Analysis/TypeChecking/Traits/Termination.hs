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
  [InstanceParam] ->
  InstanceParam ->
  Sem r ()
checkTraitTermination params arg =
  mapM_ check params
  where
    check :: InstanceParam -> Sem r ()
    check p
      | checkSubterm arg p =
          return ()
      | otherwise =
          throw (ErrTraitNotTerminating (TraitNotTerminating (paramToExpression arg)))

checkSubterm :: InstanceParam -> InstanceParam -> Bool
checkSubterm p1 p2
  | p1 == p2 = True
  | otherwise = case p2 of
      InstanceParamApp InstanceApp {..} ->
        any (checkSubterm p1) _instanceAppArgs
      InstanceParamMeta v2
        | InstanceParamVar v1 <- p1 ->
            v1 == v2
      _ ->
        False
