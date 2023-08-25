module Juvix.Compiler.Asm.Pipeline
  ( module Juvix.Compiler.Asm.Pipeline,
    module Juvix.Compiler.Asm.Data.InfoTable,
    module Juvix.Compiler.Asm.Options,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation
import Juvix.Compiler.Pipeline.EntryPoint

-- | Perform transformations on JuvixAsm necessary before the translation to
-- JuvixReg
toReg' :: (Members '[Error AsmError, Reader Options] r) => InfoTable -> Sem r InfoTable
toReg' = validate >=> computeStackUsage >=> computePrealloc

toReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => InfoTable -> Sem r InfoTable
toReg = mapReader fromEntryPoint . mapError (JuvixError @AsmError) . toReg'
