module Juvix.Compiler.Nockma.ModuleInfo where

import Juvix.Compiler.Nockma.Language
import Juvix.Prelude hiding (Path)

data ModuleInfo = ModuleInfo
  { _moduleInfoSHA256 :: Text,
    _moduleInfoFunctions :: HashMap Symbol FunctionInfo
  }
  deriving stock (Generic)

data FunctionInfo = FunctionInfo
  { _functionInfoName :: Text,
    _functionInfoPath :: Path
  }
  deriving stock (Generic)

makeLenses ''ModuleInfo
makeLenses ''FunctionInfo
