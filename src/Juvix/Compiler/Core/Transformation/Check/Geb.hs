module Juvix.Compiler.Core.Transformation.Check.Geb where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base

checkGeb :: forall r. (Member (Error CoreError) r) => InfoTable -> Sem r InfoTable
checkGeb tab =
  checkMainExists tab
    >> checkNoRecursiveTypes tab
    >> checkNoAxioms tab
    >> mapAllNodesM checkNoIO tab
    >> mapAllNodesM (checkBuiltins False) tab
    >> mapAllNodesM (checkTypes False tab) tab
