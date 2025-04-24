module Juvix.Compiler.Store.Internal.Data.InstanceInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

data InstanceParam
  = InstanceParamVar VarName
  | InstanceParamApp InstanceApp
  | InstanceParamFun InstanceFun
  | InstanceParamHole Hole
  | InstanceParamMeta VarName
  deriving stock (Eq, Generic)

instance Serialize InstanceParam

instance NFData InstanceParam

data InstanceAppHead
  = InstanceAppHeadInductive Name
  | InstanceAppHeadAxiom Name
  deriving stock (Eq, Ord, Generic)

instance Serialize InstanceAppHead

instance NFData InstanceAppHead

instance Hashable InstanceAppHead

data InstanceApp = InstanceApp
  { _instanceAppHead :: InstanceAppHead,
    _instanceAppArgs :: [InstanceParam],
    -- | The original expression from which this InstanceApp was created
    _instanceAppExpression :: Expression
  }
  deriving stock (Eq, Generic)

instance Serialize InstanceApp

instance NFData InstanceApp

instanceAppHeadName :: Lens' InstanceAppHead Name
instanceAppHeadName f = \case
  InstanceAppHeadInductive n -> InstanceAppHeadInductive <$> f n
  InstanceAppHeadAxiom n -> InstanceAppHeadAxiom <$> f n

data InstanceFun = InstanceFun
  { _instanceFunLeft :: InstanceParam,
    _instanceFunRight :: InstanceParam,
    -- | The original expression from which this InstanceFun was created
    _instanceFunExpression :: Expression
  }
  deriving stock (Eq, Generic)

instance Serialize InstanceFun

instance NFData InstanceFun

data InstanceInfo = InstanceInfo
  { _instanceInfoInductive :: InductiveName,
    -- | The parameters of the instance are the arguments of the target trait
    _instanceInfoParams :: [InstanceParam],
    -- | The identifier of the instance definition
    _instanceInfoResult :: Iden,
    -- | The arguments of the instance are the trait applications on which the
    -- instance depends
    _instanceInfoArgs :: [FunctionParameter]
  }
  deriving stock (Eq, Generic)

instance Hashable InstanceInfo where
  hashWithSalt salt InstanceInfo {..} = hashWithSalt salt _instanceInfoResult

instance Serialize InstanceInfo

instance NFData InstanceInfo

-- | Maps trait names to available instances
newtype InstanceTable = InstanceTable
  { _instanceTableMap :: HashMap InductiveName [InstanceInfo]
  }
  deriving stock (Eq, Generic)

instance Serialize InstanceTable

instance NFData InstanceTable

makeLenses ''InstanceApp
makeLenses ''InstanceFun
makeLenses ''InstanceInfo
makeLenses ''InstanceTable

instance Semigroup InstanceTable where
  t1 <> t2 =
    InstanceTable $
      HashMap.unionWith combine (t1 ^. instanceTableMap) (t2 ^. instanceTableMap)
    where
      combine :: [InstanceInfo] -> [InstanceInfo] -> [InstanceInfo]
      combine ii1 ii2 = nubHashable (ii1 ++ ii2)

instance Monoid InstanceTable where
  mempty = InstanceTable mempty
