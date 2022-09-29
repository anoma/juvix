module Juvix.Compiler.Core.Translation.FromInternal.Data.Context where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Prelude

newtype CoreResult = CoreResult
  { _coreResultTable :: Core.InfoTable
  }

makeLenses ''CoreResult
