module Juvix.Compiler.Reg.Pipeline
  ( module Juvix.Compiler.Reg.Pipeline,
    module Juvix.Compiler.Reg.Data.InfoTable,
  )
where

import Juvix.Compiler.Reg.Data.Blocks.InfoTable qualified as Blocks
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Transformation
import Juvix.Compiler.Reg.Transformation.Blocks.Liveness qualified as Blocks
import Juvix.Compiler.Reg.Translation.Blocks.FromReg qualified as Blocks

-- | Perform transformations on JuvixReg necessary before the translation to C
toC :: InfoTable -> Sem r InfoTable
toC = applyTransformations toCTransformations

-- | Perform transformations on JuvixReg necessary before the translation to Rust
toRust :: InfoTable -> Sem r InfoTable
toRust = applyTransformations toRustTransformations

-- | Perform transformations on JuvixReg necessary before the translation to
-- Cairo assembly
toCasm :: InfoTable -> Sem r Blocks.InfoTable
toCasm = applyTransformations toCasmTransformations >=> return . Blocks.computeLiveness . Blocks.fromReg
