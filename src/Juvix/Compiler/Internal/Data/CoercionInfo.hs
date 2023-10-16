module Juvix.Compiler.Internal.Data.CoercionInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data CoercionInfo = CoercionInfo
  { _coercionInfoInductive :: Name,
    _coercionInfoParams :: [InstanceParam],
    _coercionInfoTarget :: InstanceApp,
    _coercionInfoResult :: Expression,
    _coercionInfoArgs :: [FunctionParameter]
  }
  deriving stock (Eq)

instance Hashable CoercionInfo where
  hashWithSalt salt CoercionInfo {..} = hashWithSalt salt _coercionInfoResult

-- | Maps trait names to available coercions
newtype CoercionTable = CoercionTable
  { _coercionTableMap :: HashMap InductiveName [CoercionInfo]
  }

makeLenses ''CoercionInfo
makeLenses ''CoercionTable

instance Semigroup CoercionTable where
  t1 <> t2 =
    CoercionTable $
      HashMap.unionWith combine (t1 ^. coercionTableMap) (t2 ^. coercionTableMap)
    where
      combine :: [CoercionInfo] -> [CoercionInfo] -> [CoercionInfo]
      combine ii1 ii2 = nubHashable (ii1 ++ ii2)

instance Monoid CoercionTable where
  mempty = CoercionTable mempty

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

coercionFromTypedExpression :: TypedExpression -> Maybe CoercionInfo
coercionFromTypedExpression TypedExpression {..}
  | null args = Nothing
  | otherwise = do
      tgt <- traitFromExpression metaVars (t ^. paramType)
      InstanceApp {..} <- traitFromExpression metaVars e
      return $
        CoercionInfo
          { _coercionInfoInductive = _instanceAppHead,
            _coercionInfoParams = _instanceAppArgs,
            _coercionInfoTarget = tgt,
            _coercionInfoResult = _typedExpression,
            _coercionInfoArgs = args'
          }
  where
    (args, e) = unfoldFunType _typedType
    args' = init args
    t = List.last args
    metaVars = HashSet.fromList $ mapMaybe (^. paramName) args'

cyclicCoercions :: CoercionTable -> HashSet Name
cyclicCoercions ctab = nodesOnCycles depInfo
  where
    depInfo =
      createDependencyInfo
        ( HashMap.map
            (HashSet.fromList . map (^. coercionInfoTarget . instanceAppHead))
            (ctab ^. coercionTableMap)
        )
        mempty
