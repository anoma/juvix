module Juvix.Compiler.Casm.Transformation
  ( module Juvix.Compiler.Casm.Transformation.Base,
    module Juvix.Compiler.Casm.Transformation,
    module Juvix.Compiler.Casm.Data.TransformationId,
  )
where

import Juvix.Compiler.Casm.Data.TransformationId
import Juvix.Compiler.Casm.Transformation.Base
import Juvix.Compiler.Casm.Transformation.Optimize.Peephole

applyTransformations :: forall r. [TransformationId] -> Code -> Sem r Code
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> Code -> Sem r Code
    appTrans = \case
      IdentityTrans -> return
      Peephole -> return . peephole
