module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Transformation

-- | Perform transformations on Core necessary for efficient evaluation
toEval :: Member (Error JuvixError) r => InfoTable -> Sem r InfoTable
toEval = applyTransformations toEvalTransformations

-- | Perform transformations on Core necessary before the translation to
-- Core.Stripped
toStripped :: Member (Error JuvixError) r => InfoTable -> Sem r InfoTable
toStripped = applyTransformations toStrippedTransformations

-- | Perform transformations on Core necessary before the translation to GEB
toGeb :: Member (Error JuvixError) r => InfoTable -> Sem r InfoTable
toGeb = applyTransformations toGebTransformations
