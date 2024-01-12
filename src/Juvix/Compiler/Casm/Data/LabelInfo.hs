module Juvix.Compiler.Casm.Data.LabelInfo where

import Juvix.Compiler.Casm.Language

newtype LabelInfo = LabelInfo
  { _labelInfoTable :: HashMap Symbol Address
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''LabelInfo
