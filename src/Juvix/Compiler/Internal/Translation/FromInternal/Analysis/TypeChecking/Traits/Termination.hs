module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Termination
  ( checkTraitTermination,
    checkCoercionInfo,
  )
where

import Juvix.Compiler.Internal.Extra.CoercionInfo
import Juvix.Compiler.Internal.Extra.InstanceInfo
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude

-- | `checkTraitTermination arg inst` checks if the instance argument `arg` is
-- smaller than the target of the instance `inst`, by checking that the multiset
-- of the arguments of `arg` is smaller than the multiset of the trait instance
-- parameters of `inst` according to the Dershowitz-Manna multiset ordering
-- extension of the subterm ordering.
checkTraitTermination ::
  forall r.
  (Member (Error TypeCheckerError) r) =>
  InstanceApp ->
  InstanceInfo ->
  Sem r ()
checkTraitTermination InstanceApp {..} InstanceInfo {..}
  | checkMultisetOrdering _instanceAppArgs _instanceInfoParams =
      return ()
  | otherwise =
      throw (ErrTraitNotTerminating (TraitNotTerminating _instanceAppExpression))

checkCoercionInfo :: CoercionInfo -> CoercionInfo
checkCoercionInfo ci@CoercionInfo {..} =
  ci
    { _coercionInfoDecreasing =
        checkMultisetOrdering (_coercionInfoTarget ^. instanceAppArgs) _coercionInfoParams
    }

-- Checks the Dershowitz-Manna multiset ordering extension of the subterm
-- ordering, according to the following characterization:
--
-- N <_mul M iff M /= N and for all x in N \ M, exists y in M \ N s.t. x < y
--
-- Any reduction ordering could be used instead of the subterm ordering.
--
-- See: Baader, Nipkow, "Term Rewriting and All That", Section 2.6.2
checkMultisetOrdering ::
  [InstanceParam] ->
  [InstanceParam] ->
  Bool
checkMultisetOrdering ms1 ms2
  | null ms1' && null ms2' = False
  | otherwise =
      all (\x -> any (checkStrictSubterm x) ms2') ms1'
  where
    ms1' = mdiff ms1 ms2
    ms2' = mdiff ms2 ms1

    mdiff :: [InstanceParam] -> [InstanceParam] -> [InstanceParam]
    mdiff xs = foldr delete xs

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
