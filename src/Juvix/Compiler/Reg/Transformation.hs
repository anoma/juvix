module Juvix.Compiler.Reg.Transformation
  ( module Juvix.Compiler.Reg.Transformation.Base,
    module Juvix.Compiler.Reg.Transformation,
    module Juvix.Compiler.Reg.Data.TransformationId,
  )
where

import Juvix.Compiler.Reg.Data.TransformationId
import Juvix.Compiler.Reg.Transformation.Base
import Juvix.Compiler.Reg.Transformation.Cleanup
import Juvix.Compiler.Reg.Transformation.ConstantPropagation (constantPropagate)
import Juvix.Compiler.Reg.Transformation.CopyPropagation
import Juvix.Compiler.Reg.Transformation.IdentityTrans
import Juvix.Compiler.Reg.Transformation.InitBranchVars
import Juvix.Compiler.Reg.Transformation.SSA

applyTransformations :: forall r. [TransformationId] -> InfoTable -> Sem r InfoTable
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> Sem r InfoTable
    appTrans = \case
      IdentityTrans -> return . identity
      Cleanup -> return . cleanup
      SSA -> return . computeSSA
      InitBranchVars -> return . initBranchVars
      CopyPropagation -> return . copyPropagate
      ConstantPropagation -> return . constantPropagate
