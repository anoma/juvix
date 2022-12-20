module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Transformation

-- | Perform transformations on Core necessary before the translation to
-- Core.Stripped
toStripped :: InfoTable -> InfoTable
toStripped = applyTransformations [NatToInt, LambdaLifting, MoveApps, TopEtaExpand, RemoveTypeArgs]
