module Juvix.Compiler.Asm.Pipeline where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Language

-- | Perform transformations on JuvixAsm necessary before the translation to
-- JuvixReg
toReg :: Member (Error AsmError) r => InfoTable -> Sem r InfoTable
toReg = validateInfoTable >=> computeStackUsage
