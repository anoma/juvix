module Juvix.Compiler.FFI.Anoma where

import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Prelude

data Anoma = Anoma
  { _anomaStdlibCall :: Nockma.Term Natural
  }
  deriving stock (Generic, Show)

instance NFData Anoma

instance Serialize Anoma

makeLenses ''Anoma
