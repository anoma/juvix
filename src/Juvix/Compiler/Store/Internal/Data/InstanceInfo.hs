module Juvix.Compiler.Store.Internal.Data.InstanceInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty.Base
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

instance HasAtomicity InstanceParam where
  atomicity = \case
    InstanceParamVar v -> atomicity v
    InstanceParamMeta v -> atomicity v
    InstanceParamHole h -> atomicity h
    InstanceParamApp h -> atomicity h
    InstanceParamFun h -> atomicity h

data InstanceApp = InstanceApp
  { _instanceAppHead :: Name,
    _instanceAppArgs :: [InstanceParam],
    -- | The original expression from which this InstanceApp was created
    _instanceAppExpression :: Expression
  }
  deriving stock (Eq, Generic)

instance HasAtomicity InstanceApp where
  atomicity = const (Aggregate appFixity)

instance Serialize InstanceApp

instance NFData InstanceApp

data InstanceFun = InstanceFun
  { _instanceFunLeft :: InstanceParam,
    _instanceFunRight :: InstanceParam,
    -- | The original expression from which this InstanceFun was created
    _instanceFunExpression :: Expression
  }
  deriving stock (Eq, Generic)

instance HasAtomicity InstanceFun where
  atomicity = const (Aggregate funFixity)

instance Serialize InstanceFun

instance NFData InstanceFun

data InstanceInfo = InstanceInfo
  { _instanceInfoInductive :: InductiveName,
    _instanceInfoParams :: [InstanceParam],
    _instanceInfoIden :: Iden,
    _instanceInfoArgs :: [FunctionParameter]
  }
  deriving stock (Eq, Generic)

instance Hashable InstanceInfo where
  hashWithSalt salt InstanceInfo {..} = hashWithSalt salt _instanceInfoIden

instance HasLoc InstanceInfo where
  getLoc InstanceInfo {..} = getLoc _instanceInfoIden

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

instance PrettyCode InstanceInfo where
  ppCode = ppCode . (^. instanceInfoIden)

instance Semigroup InstanceTable where
  t1 <> t2 =
    InstanceTable $
      HashMap.unionWith combine (t1 ^. instanceTableMap) (t2 ^. instanceTableMap)
    where
      combine :: [InstanceInfo] -> [InstanceInfo] -> [InstanceInfo]
      combine ii1 ii2 = nubHashable (ii1 ++ ii2)

instance Monoid InstanceTable where
  mempty = InstanceTable mempty

paramToExpression :: InstanceParam -> Expression
paramToExpression = \case
  InstanceParamVar v ->
    ExpressionIden (IdenVar v)
  InstanceParamApp InstanceApp {..} ->
    _instanceAppExpression
  InstanceParamFun InstanceFun {..} ->
    _instanceFunExpression
  InstanceParamHole h ->
    ExpressionHole h
  InstanceParamMeta v ->
    ExpressionIden (IdenVar v)

instance PrettyCode InstanceParam where
  ppCode = ppCode . paramToExpression
