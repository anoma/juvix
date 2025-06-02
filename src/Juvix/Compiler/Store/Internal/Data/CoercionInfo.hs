module Juvix.Compiler.Store.Internal.Data.CoercionInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Extra.Serialize
import Juvix.Prelude

data CoercionInfo = CoercionInfo
  { -- | The inductive name of the trait on the right of `:`
    _coercionInfoInductive :: InductiveName,
    -- | The parameters of the coercion are the arguments of the trait on the right of `:`
    _coercionInfoParams :: [InstanceParam],
    -- | The target instance argument of the coercion instance
    _coercionInfoTarget :: InstanceApp,
    -- | The identifier of the coercion instance definition
    _coercionInfoResult :: Iden,
    -- | Remaining instance arguments (not including the target)
    _coercionInfoArgs :: [FunctionParameter],
    _coercionInfoDecreasing :: Bool
  }
  deriving stock (Eq, Generic)

instance Hashable CoercionInfo where
  hashWithSalt salt CoercionInfo {..} = hashWithSalt salt _coercionInfoResult

instance Serialize CoercionInfo

instance NFData CoercionInfo

-- | Maps trait names to available coercions
newtype CoercionTable = CoercionTable
  { _coercionTableMap :: HashMap InductiveName [CoercionInfo]
  }
  deriving stock (Eq, Generic)

instance Serialize CoercionTable

instance NFData CoercionTable

makeLenses ''CoercionInfo
makeLenses ''CoercionTable

instance Semigroup CoercionTable where
  t1 <> t2 =
    CoercionTable
      $ HashMap.unionWith combine (t1 ^. coercionTableMap) (t2 ^. coercionTableMap)
    where
      combine :: [CoercionInfo] -> [CoercionInfo] -> [CoercionInfo]
      combine ii1 ii2 = nubHashable (ii1 ++ ii2)

instance Monoid CoercionTable where
  mempty = CoercionTable mempty
