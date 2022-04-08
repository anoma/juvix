module MiniJuvix.Syntax.Concrete.Parser.InfoTable where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Parser.ParsedItem

newtype InfoTable = InfoTable
  { _infoParsedItems :: [ParsedItem]
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Eq, Show)

makeLenses ''InfoTable
