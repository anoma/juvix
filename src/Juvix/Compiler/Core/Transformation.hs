module Juvix.Compiler.Core.Transformation
  ( module Juvix.Compiler.Core.Transformation.Base,
    module Juvix.Compiler.Core.Transformation,
    module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.LambdaLetRecLifting,
    module Juvix.Compiler.Core.Transformation.TopEtaExpand,
    module Juvix.Compiler.Core.Data.TransformationId,
  )
where

import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo
import Juvix.Compiler.Core.Transformation.ConvertBuiltinTypes
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Transformation.Identity
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
import Juvix.Compiler.Core.Transformation.MoveApps
import Juvix.Compiler.Core.Transformation.NatToInt
import Juvix.Compiler.Core.Transformation.RemoveTypeArgs
import Juvix.Compiler.Core.Transformation.TopEtaExpand
import Juvix.Compiler.Core.Transformation.UnrollRecursion

applyTransformations :: [TransformationId] -> InfoTable -> InfoTable
applyTransformations ts tbl = foldl' (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> InfoTable
    appTrans = \case
      LambdaLetRecLifting -> lambdaLifting
      Identity -> identity
      TopEtaExpand -> topEtaExpand
      RemoveTypeArgs -> removeTypeArgs
      MoveApps -> moveApps
      NatToInt -> natToInt
      ConvertBuiltinTypes -> convertBuiltinTypes
      ComputeTypeInfo -> computeTypeInfo
      UnrollRecursion -> unrollRecursion
