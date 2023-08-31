module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Extra where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error
import Juvix.Prelude

type SubsI = HashMap VarName InstanceParam

lookupInstance' ::
  InstanceTable ->
  Name ->
  [InstanceParam] ->
  [(InstanceInfo, SubsI)]
lookupInstance' tab name params = do
  let is = fromMaybe [] $ HashMap.lookup name tab
  mapMaybe matchInstance is
  where
    matchInstance :: InstanceInfo -> Maybe (InstanceInfo, SubsI)
    matchInstance ii@InstanceInfo {..}
      | length params /= length _instanceInfoParams =
          Nothing
      | otherwise =
          let (mp, b) =
                run $
                  runState mempty $
                    and <$> sequence (zipWithExact goMatch _instanceInfoParams params)
           in if
                  | b -> Just (ii, mp)
                  | otherwise -> Nothing

    goMatch :: (Member (State SubsI) r) => InstanceParam -> InstanceParam -> Sem r Bool
    goMatch pat t = case (pat, t) of
      (InstanceParamMeta (InstanceMeta v), _) -> do
        m <- gets (HashMap.lookup v)
        case m of
          Just t'
            | t' == t ->
                return True
            | otherwise ->
                return False
          Nothing -> do
            modify (HashMap.insert v t)
            return True
      (InstanceParamVar (InstanceVar v1), InstanceParamVar (InstanceVar v2))
        | v1 == v2 ->
            return True
      (InstanceParamApp (InstanceApp h1 args1), InstanceParamApp (InstanceApp h2 args2))
        | h1 == h2 -> do
            and <$> sequence (zipWithExact goMatch args1 args2)
      _ ->
        return False

lookupInstance ::
  forall r.
  (Member (Error TraitError) r) =>
  InstanceTable ->
  Expression ->
  Sem r [(InstanceInfo, SubsI)]
lookupInstance tab ty = do
  case traitFromExpression mempty ty of
    Just (InstanceApp h args) ->
      return $ lookupInstance' tab h args
    _ ->
      throw (ErrNotATrait (NotATrait ty))

substParam :: SubsI -> InstanceParam -> InstanceParam
substParam subs p = case p of
  InstanceParamVar {} -> p
  InstanceParamApp app ->
    InstanceParamApp (over instanceAppArgs (map (substParam subs)) app)
  InstanceParamMeta (InstanceMeta v) ->
    fromMaybe p (HashMap.lookup v subs)

subsIToE :: SubsI -> SubsE
subsIToE = fmap paramToExpression

isTrait :: InfoTable -> InductiveName -> Bool
isTrait tab name = fromJust (HashMap.lookup name (tab ^. infoInductives)) ^. inductiveInfoDef . inductiveTrait
