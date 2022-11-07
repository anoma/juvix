module Juvix.Compiler.Asm.Pipeline where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation

-- | Perform transformations on JuvixAsm necessary before the translation to
-- JuvixReg
toReg :: Members '[Error AsmError, Reader Options] r => InfoTable -> Sem r InfoTable
toReg = validate >=> computeStackUsage >=> computePrealloc
