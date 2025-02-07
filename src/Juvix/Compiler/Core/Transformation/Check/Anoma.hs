module Juvix.Compiler.Core.Transformation.Check.Anoma where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base

checkAnoma :: forall r. (Member (Error CoreError) r) => Module -> Sem r Module
checkAnoma md = do
  checkMainExists md
  checkNoAxioms md
  mapAllNodesM checkNoIO md
  mapAllNodesM (checkBuiltins' ([OpStrToInt, OpShow] ++ builtinsCairo) [PrimField]) md
