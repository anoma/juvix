module Juvix.Compiler.Builtins.Ordering where

import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkOrderingDef :: forall r. InductiveDef -> Sem r ()
checkOrderingDef _d = return ()
