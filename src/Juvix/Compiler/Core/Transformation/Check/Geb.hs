module Juvix.Compiler.Core.Transformation.Check.Geb where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base

checkGeb :: forall r. (Member (Error CoreError) r) => Module -> Sem r Module
checkGeb md =
  checkMainExists md
    >> checkNoRecursiveTypes md
    >> checkNoAxioms md
    >> mapAllNodesM checkNoIO md
    >> mapAllNodesM (checkBuiltins False) md
    >> mapAllNodesM (checkTypes False md) md
