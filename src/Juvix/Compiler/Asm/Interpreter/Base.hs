module Juvix.Compiler.Asm.Interpreter.Base
  ( module Juvix.Compiler.Tree.Language.Value,
    module Juvix.Compiler.Asm.Language,
    Val,
  )
where

import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Language.Value hiding (Value)
import Juvix.Compiler.Tree.Language.Value qualified as Tree

type Val = Tree.Value
