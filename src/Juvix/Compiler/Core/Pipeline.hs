module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Pipeline.EntryPoint (EntryPoint)

-- | Perform transformations on Core necessary for efficient evaluation
toEval' :: Members '[Error JuvixError, Reader CoreOptions] r => InfoTable -> Sem r InfoTable
toEval' = applyTransformations toEvalTransformations

toTypechecked :: Members '[Error JuvixError, Reader EntryPoint] r => InfoTable -> Sem r InfoTable
toTypechecked = mapReader fromEntryPoint . applyTransformations toTypecheckTransformations

toEval :: Members '[Error JuvixError, Reader EntryPoint] r => InfoTable -> Sem r InfoTable
toEval = mapReader fromEntryPoint . applyTransformations toEvalTransformations

-- | Perform transformations on Core necessary before the translation to
-- Core.Stripped
toStripped' :: Members '[Error JuvixError, Reader CoreOptions] r => InfoTable -> Sem r InfoTable
toStripped' = applyTransformations toStrippedTransformations

toStripped :: Members '[Error JuvixError, Reader EntryPoint] r => InfoTable -> Sem r InfoTable
toStripped = mapReader fromEntryPoint . applyTransformations toStrippedTransformations

-- | Perform transformations on Core necessary before the translation to GEB
toGeb' :: Members '[Error JuvixError, Reader CoreOptions] r => InfoTable -> Sem r InfoTable
toGeb' = applyTransformations toGebTransformations

toGeb :: Members '[Error JuvixError, Reader EntryPoint] r => InfoTable -> Sem r InfoTable
toGeb = mapReader fromEntryPoint . applyTransformations toGebTransformations
