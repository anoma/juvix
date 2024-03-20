module Juvix.Compiler.Core.Transformation.Check.Cairo where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base

checkCairo :: forall r. (Member (Error CoreError) r) => Module -> Sem r Module
checkCairo md = do
  checkMainExists md
  checkNoAxioms md
  mapAllNodesM checkNoIO md
  mapAllNodesM (checkBuiltins' [OpStrConcat, OpStrToInt, OpShow] [PrimString]) md
