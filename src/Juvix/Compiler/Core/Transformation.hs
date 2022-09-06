module Juvix.Compiler.Core.Transformation
  ( module Juvix.Compiler.Core.Transformation.Base,
    module Juvix.Compiler.Core.Transformation,
    module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Data.TransformationId,
  )
where

import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Transformation.LambdaLifting

applyTransformations :: [TransformationId] -> InfoTable -> InfoTable
applyTransformations ts tbl = foldl' (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> InfoTable
    appTrans = \case
      LambdaLifting -> lambdaLifting
