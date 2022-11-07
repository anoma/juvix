module Juvix.Compiler.Core.Translation.FromInternal.Data.Context where

import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Prelude

data CoreResult = CoreResult
  { _coreResultTable :: Core.InfoTable,
    _coreResultInternalTypedResult :: Internal.InternalTypedResult
  }

makeLenses ''CoreResult
