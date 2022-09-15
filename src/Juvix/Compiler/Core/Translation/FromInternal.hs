module Juvix.Compiler.Core.Translation.FromInternal where

import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Internal.Translation qualified as Internal
import Juvix.Prelude

fromInternal :: Internal.InternalTypedResult -> Sem r CoreResult
fromInternal = undefined
