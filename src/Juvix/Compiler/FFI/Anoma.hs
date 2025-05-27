module Juvix.Compiler.FFI.Anoma where

import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Nockma.Language qualified as Nockma

data Anoma = Anoma
  { _anomaSymbol :: Symbol,
    _anomaStdlibCall :: Nockma.Term Natural,
    _anomaCode :: Maybe Core.Node
  }

makeLenses ''Anoma
