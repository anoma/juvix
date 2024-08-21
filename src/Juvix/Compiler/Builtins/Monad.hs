module Juvix.Compiler.Builtins.Monad where

import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkMonadBind :: forall r. FunctionDef -> Sem r ()
checkMonadBind _ = return ()
