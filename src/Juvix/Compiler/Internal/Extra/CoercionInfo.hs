module Juvix.Compiler.Internal.Extra.CoercionInfo
  ( module Juvix.Compiler.Store.Internal.Data.CoercionInfo,
    module Juvix.Compiler.Internal.Extra.CoercionInfo,
    module Juvix.Compiler.Internal.Data.TypedIden,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Internal.Data.TypedIden
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Extra.InstanceInfo
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Data.CoercionInfo
import Juvix.Prelude

updateCoercionTable :: CoercionTable -> CoercionInfo -> CoercionTable
updateCoercionTable tab ci@CoercionInfo {..} =
  over coercionTableMap (HashMap.alter go _coercionInfoInductive) tab
  where
    go :: Maybe [CoercionInfo] -> Maybe [CoercionInfo]
    go = \case
      Just is -> Just (ci : is)
      Nothing -> Just [ci]

lookupCoercionTable :: CoercionTable -> Name -> Maybe [CoercionInfo]
lookupCoercionTable tab name = HashMap.lookup name (tab ^. coercionTableMap)

coercionFromTypedIden :: TypedIden -> Sem r CoercionInfo
coercionFromTypedIden TypedIden {..}
  | null args = Nothing
  | otherwise = do
      tgt <- traitFromExpression metaVars (t ^. paramType)
      InstanceApp {..} <- traitFromExpression metaVars e
      return $
        CoercionInfo
          { _coercionInfoInductive = _instanceAppHead ^. instanceAppHeadName,
            _coercionInfoParams = _instanceAppArgs,
            _coercionInfoTarget = tgt,
            _coercionInfoResult = _typedIden,
            _coercionInfoArgs = args',
            _coercionInfoDecreasing = False
          }
  where
    (args, e) = unfoldFunType _typedIdenType
    args' = init args
    t = List.last args
    metaVars = HashSet.fromList $ mapMaybe (^. paramName) args'

cyclicCoercions :: CoercionTable -> HashSet Name
cyclicCoercions ctab = nodesOnCycles depInfo
  where
    depInfo =
      createDependencyInfo
        ( HashMap.map
            (hashSet . map (^. coercionInfoTarget . instanceAppHead . instanceAppHeadName))
            (ctab ^. coercionTableMap)
        )
        mempty
