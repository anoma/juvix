module Juvix.Compiler.Pipeline.ImportParents where

import Juvix.Compiler.Concrete.Data
import Juvix.Prelude.Base

newtype ImportParents = ImportParents
  { _importParents :: [TopModulePath]
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''ImportParents
