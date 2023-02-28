module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Transformation

-- | Perform transformations on Core necessary for efficient evaluation
toEval :: InfoTable -> InfoTable
toEval = applyTransformations toEvalTransformations

-- | Perform transformations on Core necessary before the translation to
-- Core.Stripped
toStripped :: InfoTable -> InfoTable
toStripped = applyTransformations toStrippedTransformations

-- | Perform transformations on Core necessary before the translation to GEB
toGeb :: InfoTable -> InfoTable
toGeb = applyTransformations toGebTransformations
