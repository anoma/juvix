module Juvix.Compiler.Internal.Data.CoercionInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

data CoercionInfo = CoercionInfo
  { _coercionInfoInductive :: Name,
    _coercionInfoParams :: [InstanceParam],
    _coercionInfoTarget :: InstanceApp,
    _coercionInfoResult :: Expression,
    _coercionInfoArgs :: [FunctionParameter]
  }
  deriving stock (Eq, Generic)

instance Hashable CoercionInfo where
  hashWithSalt salt CoercionInfo {..} = hashWithSalt salt _coercionInfoResult

instance Serialize CoercionInfo

-- | Maps trait names to available coercions
newtype CoercionTable = CoercionTable
  { _coercionTableMap :: HashMap InductiveName [CoercionInfo]
  }
  deriving stock (Eq, Generic)

instance Serialize CoercionTable

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
