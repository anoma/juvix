module Juvix.Compiler.Tree.Pipeline
  ( module Juvix.Compiler.Tree.Pipeline,
    module Juvix.Compiler.Tree.Data.InfoTable,
  )
where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Transformation

toNockma :: (Members '[Error JuvixError, Reader EntryPoint] r) => InfoTable -> Sem r InfoTable
toNockma = mapReader fromEntryPoint . applyTransformations toNockmaTransformations

toAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => InfoTable -> Sem r InfoTable
toAsm = mapReader fromEntryPoint . applyTransformations toAsmTransformations

toCairoAsm :: (Members '[Error JuvixError, Reader EntryPoint] r) => InfoTable -> Sem r InfoTable
toCairoAsm = mapReader fromEntryPoint . applyTransformations toCairoAsmTransformations
