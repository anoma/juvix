module Juvix.Compiler.Core.Transformation.Check.Exec where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base

checkExec :: forall r. (Member (Error CoreError) r) => Module -> Sem r Module
checkExec md = do
  checkNoAxioms md
  checkMainExists md
  checkMainTypeExec md
  mapAllNodesM (checkBuiltins' (builtinsCairo ++ builtinsAnoma ++ builtinsByteArray) [PrimByteArray]) md
