module Juvix.Compiler.Core.Transformation.Check.Rust where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base

checkRust :: forall r. (Member (Error CoreError) r) => Module -> Sem r Module
checkRust = checkAll $ \md -> do
  checkNoAxioms md
  checkMainExists md
  checkMainTypeExec md
  mapAllNodesM checkNoIO md
  mapAllNodesM (checkBuiltins False) md
