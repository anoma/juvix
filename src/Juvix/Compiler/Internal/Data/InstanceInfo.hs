module Juvix.Compiler.Internal.Data.InstanceInfo where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data InstanceParam
  = InstanceParamVar InstanceVar
  | InstanceParamApp InstanceApp
  | InstanceParamMeta InstanceMeta
  deriving stock (Eq)

newtype InstanceVar = InstanceVar
  { _instanceVarName :: VarName
  }
  deriving stock (Eq)

newtype InstanceMeta = InstanceMeta
  { _instanceMetaName :: VarName
  }
  deriving stock (Eq)

data InstanceApp = InstanceApp
  { _instanceAppHead :: InductiveName,
    _instanceAppArgs :: [InstanceParam]
  }
  deriving stock (Eq)

data InstanceInfo = InstanceInfo
  { _instanceInfoInductive :: InductiveName,
    _instanceInfoParams :: [InstanceParam],
    _instanceInfoResult :: Expression,
    _instanceInfoArgs :: [FunctionParameter]
  }

-- | Maps trait names to available instances
type InstanceTable = HashMap InductiveName [InstanceInfo]

makeLenses ''InstanceVar
makeLenses ''InstanceApp
makeLenses ''InstanceMeta
makeLenses ''InstanceInfo
