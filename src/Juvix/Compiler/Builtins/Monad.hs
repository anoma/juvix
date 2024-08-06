module Juvix.Compiler.Builtins.Monad where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerMonadBind :: forall r. (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerMonadBind f = registerBuiltin BuiltinMonadBind (f ^. funDefName)
