module Juvix.Compiler.Casm.Language.Base
  ( module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Casm.Language.Base,
  )
where

import Juvix.Compiler.Core.Language.Base hiding (Ap, Index)

type Offset = Int16

type Address = Int

data Reg
  = Ap
  | Fp
  deriving stock (Eq, Show)
