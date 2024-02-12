module Juvix.Compiler.Tree.Transformation.Identity
  ( module Juvix.Compiler.Tree.Transformation.Identity,
    module Juvix.Compiler.Tree.Transformation.Base,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

identity :: InfoTable -> InfoTable
identity = run . mapT' (const return)

identityU :: InfoTable -> InfoTable
identityU = run . mapT' (const (return . umap id))

identityD :: InfoTable -> InfoTable
identityD = run . mapT' (const (return . dmap id))
