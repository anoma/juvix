module Juvix.Compiler.Asm.Extra
  ( module Juvix.Compiler.Asm.Extra,
    module Juvix.Compiler.Asm.Error,
    module Juvix.Compiler.Asm.Extra.Base,
    module Juvix.Compiler.Asm.Extra.Type,
    module Juvix.Compiler.Asm.Extra.Recursors,
  )
where

import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Recursors
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language

validate :: forall r. Member (Error AsmError) r => Arguments -> Code -> Sem r ()
validate args = void . recurse sig args
  where
    sig :: RecursorSig r ()
    sig =
      RecursorSig
        { _recurseInstr = \_ _ -> return (),
          _recurseBranch = \_ _ _ _ -> return (),
          _recurseCase = \_ _ _ _ -> return ()
        }
