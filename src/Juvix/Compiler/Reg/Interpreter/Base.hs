module Juvix.Compiler.Reg.Interpreter.Base
  ( module Juvix.Compiler.Tree.Language.Value,
    module Juvix.Compiler.Reg.Language,
    Val,
  )
where

import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Tree.Language.Value hiding (Value)
import Juvix.Compiler.Tree.Language.Value qualified as Tree

type Val = Tree.Value
