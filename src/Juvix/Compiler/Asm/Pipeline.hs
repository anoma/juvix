module Juvix.Compiler.Asm.Pipeline
  ( module Juvix.Compiler.Asm.Pipeline,
    module Juvix.Compiler.Asm.Data.InfoTable,
    module Juvix.Compiler.Asm.Options,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

-- | Perform transformations on JuvixAsm necessary before the translation to
-- JuvixReg
toReg' :: (Members '[Error AsmError, Reader Options] r) => InfoTable -> Sem r InfoTable
toReg' = validate >=> filterUnreachable >=> computeStackUsage >=> computePrealloc

-- | Perform transformations on JuvixAsm necessary before the translation to
-- Nockma
toNockma' :: (Members '[Error AsmError, Reader Options] r) => InfoTable -> Sem r InfoTable
toNockma' = validate

toReg :: (Members '[Error JuvixError, Reader EntryPoint] r) => InfoTable -> Sem r InfoTable
toReg = mapReader fromEntryPoint . mapError (JuvixError @AsmError) . toReg'

toNockma :: (Members '[Error JuvixError, Reader EntryPoint] r) => InfoTable -> Sem r InfoTable
toNockma = mapReader fromEntryPoint . mapError (JuvixError @AsmError) . toNockma'
