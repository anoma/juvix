module Juvix.Compiler.Tree.Pipeline
  ( module Juvix.Compiler.Tree.Pipeline,
    module Juvix.Compiler.Tree.Data.InfoTable,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Transformation

toNockma :: (Member (Error JuvixError) r) => InfoTable -> Sem r InfoTable
toNockma = applyTransformations toNockmaTransformations

toAsm :: (Member (Error JuvixError) r) => InfoTable -> Sem r InfoTable
toAsm = applyTransformations toAsmTransformations

toCairoAsm :: (Member (Error JuvixError) r) => InfoTable -> Sem r InfoTable
toCairoAsm = applyTransformations toCairoAsmTransformations
