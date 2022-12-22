module Juvix.Compiler.Core.Transformation
  ( module Juvix.Compiler.Core.Transformation.Base,
    module Juvix.Compiler.Core.Transformation,
    module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.TopEtaExpand,
    module Juvix.Compiler.Core.Data.TransformationId,
  )
where

import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.ConvertBuiltinTypes
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Transformation.Identity
import Juvix.Compiler.Core.Transformation.LambdaLifting
import Juvix.Compiler.Core.Transformation.MoveApps
import Juvix.Compiler.Core.Transformation.NatToInt
import Juvix.Compiler.Core.Transformation.RemoveTypeArgs
import Juvix.Compiler.Core.Transformation.TopEtaExpand

applyTransformations :: [TransformationId] -> InfoTable -> InfoTable
applyTransformations ts tbl = foldl' (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> InfoTable
    appTrans = \case
      LambdaLifting -> lambdaLifting
      Identity -> identity
      TopEtaExpand -> topEtaExpand
      RemoveTypeArgs -> removeTypeArgs
      MoveApps -> moveApps
      NatToInt -> natToInt
      ConvertBuiltinTypes -> convertBuiltinTypes
