module Juvix.Compiler.Reg.Pipeline
  ( module Juvix.Compiler.Reg.Pipeline,
    module Juvix.Compiler.Reg.Data.InfoTable,
  )
where

import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Transformation

-- | Perform transformations on JuvixReg necessary before the translation to C
toC :: InfoTable -> Sem r InfoTable
toC = applyTransformations toCTransformations

-- | Perform transformations on JuvixReg necessary before the translation to
-- Cairo assembly
toCasm :: InfoTable -> Sem r InfoTable
toCasm = applyTransformations toCasmTransformations
