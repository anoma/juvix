module Juvix.Compiler.Concrete.Data.Stage where

import Data.Kind qualified as GHC
import Juvix.Prelude

data Stage
  = Parsed
  | Scoped
  deriving stock (Eq, Show)

type AnyStage (k :: Stage -> GHC.Type) =
  Σ Stage (TyCon1 k)

$(genSingletons [''Stage])
