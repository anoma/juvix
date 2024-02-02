module Juvix.Compiler.Tree.Pipeline
  ( module Juvix.Compiler.Tree.Pipeline,
    module Juvix.Compiler.Tree.Data.InfoTable,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Transformation

toNockma :: InfoTable -> Sem r InfoTable
toNockma = applyTransformations toNockmaTransformations

toAsm :: InfoTable -> Sem r InfoTable
toAsm = applyTransformations toAsmTransformations
