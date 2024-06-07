module Juvix.Compiler.Core.Transformation.IdentityTrans
  ( module Juvix.Compiler.Core.Transformation.IdentityTrans,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Transformation.Base

identity :: Module -> Module
identity = run . mapT' (const return)
