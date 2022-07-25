module Juvix.Parsing.InfoTable where

import Juvix.Parsing.ParsedItem
import Juvix.Prelude

newtype InfoTable = InfoTable
  { _infoParsedItems :: [ParsedItem]
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Eq, Show)

makeLenses ''InfoTable
