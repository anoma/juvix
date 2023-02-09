module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Transformation

toEvalTransformations :: [TransformationId]
toEvalTransformations = [MatchToCase, NatToInt, ConvertBuiltinTypes]

-- | Perform transformations on Core necessary for efficient evaluation
toEval :: InfoTable -> InfoTable
toEval = applyTransformations toEvalTransformations

toStrippedTransformations :: [TransformationId]
toStrippedTransformations = toEvalTransformations ++ [LambdaLetRecLifting, MoveApps, TopEtaExpand, RemoveTypeArgs]

-- | Perform transformations on Core necessary before the translation to
-- Core.Stripped
toStripped :: InfoTable -> InfoTable
toStripped = applyTransformations toStrippedTransformations

toGebTransformations :: [TransformationId]
toGebTransformations = toEvalTransformations ++ [UnrollRecursion, ComputeTypeInfo]

-- | Perform transformations on Core necessary before the translation to GEB
toGeb :: InfoTable -> InfoTable
toGeb = applyTransformations toGebTransformations
