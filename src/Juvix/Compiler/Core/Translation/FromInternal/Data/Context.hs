module Juvix.Compiler.Core.Translation.FromInternal.Data.Context where

import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Prelude

data CoreResult = CoreResult
  { _coreResultModule :: Core.Module,
    _coreResultInternalTypedResult :: Internal.InternalTypedResult
  }

makeLenses ''CoreResult
