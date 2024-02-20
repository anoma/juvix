module Juvix.Compiler.Reg.Transformation
  ( module Juvix.Compiler.Reg.Transformation.Base,
    module Juvix.Compiler.Reg.Transformation,
    module Juvix.Compiler.Reg.Data.TransformationId,
  )
where

import Juvix.Compiler.Reg.Data.TransformationId
import Juvix.Compiler.Reg.Transformation.Base
import Juvix.Compiler.Reg.Transformation.Cleanup
import Juvix.Compiler.Reg.Transformation.Identity
import Juvix.Compiler.Reg.Transformation.SSA

applyTransformations :: forall r. [TransformationId] -> InfoTable -> Sem r InfoTable
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> Sem r InfoTable
    appTrans = \case
      Identity -> return . identity
      SSA -> return . computeSSA
      Cleanup -> return . cleanup
