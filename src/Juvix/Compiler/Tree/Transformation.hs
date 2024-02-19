module Juvix.Compiler.Tree.Transformation
  ( module Juvix.Compiler.Tree.Transformation.Base,
    module Juvix.Compiler.Tree.Transformation,
    module Juvix.Compiler.Tree.Data.TransformationId,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Data.TransformationId
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Transformation.Apply
import Juvix.Compiler.Tree.Transformation.Base
import Juvix.Compiler.Tree.Transformation.FilterUnreachable
import Juvix.Compiler.Tree.Transformation.Identity
import Juvix.Compiler.Tree.Transformation.TempHeight
import Juvix.Compiler.Tree.Transformation.Validate

applyTransformations :: forall r. (Member (Error JuvixError) r) => [TransformationId] -> InfoTable -> Sem r InfoTable
applyTransformations ts tbl = foldM (flip appTrans) tbl ts
  where
    appTrans :: TransformationId -> InfoTable -> Sem r InfoTable
    appTrans = \case
      Identity -> return . identity
      IdentityU -> return . identityU
      IdentityD -> return . identityD
      Apply -> return . computeApply
      TempHeight -> return . computeTempHeight
      FilterUnreachable -> return . filterUnreachable
      Validate -> mapError (JuvixError @TreeError) . validate
