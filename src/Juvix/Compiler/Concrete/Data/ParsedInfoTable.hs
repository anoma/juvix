module Juvix.Compiler.Concrete.Data.ParsedInfoTable where

import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Prelude

newtype InfoTable = InfoTable
  { _infoParsedItems :: [ParsedItem]
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Eq, Show)

makeLenses ''InfoTable
