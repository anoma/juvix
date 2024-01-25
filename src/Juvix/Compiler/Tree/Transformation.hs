module Juvix.Compiler.Tree.Transformation
  ( module Juvix.Compiler.Tree.Transformation.Base,
    module Juvix.Compiler.Tree.Transformation,
    module Juvix.Compiler.Tree.Data.TransformationId,
  )
where

import Juvix.Compiler.Tree.Data.TransformationId
import Juvix.Compiler.Tree.Transformation.Apply
import Juvix.Compiler.Tree.Transformation.Base
import Juvix.Compiler.Tree.Transformation.Identity
import Juvix.Compiler.Tree.Transformation.TempHeight

applyTransformations :: forall r. [TransformationId] -> InfoTable -> Sem r InfoTable
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> Sem r InfoTable
    appTrans = \case
      Identity -> return . identity
      IdentityU -> return . identityU
      IdentityD -> return . identityD
      Apply -> return . computeApply
      TempHeight -> return . computeTempHeight
