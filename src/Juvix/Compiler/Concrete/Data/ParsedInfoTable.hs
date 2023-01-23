module Juvix.Compiler.Concrete.Data.ParsedInfoTable where

import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Data.Comment
import Juvix.Prelude

data InfoTable = InfoTable
  { _infoParsedItems :: [ParsedItem],
    _infoParsedComments :: Comments
  }
  deriving stock (Eq, Show)

makeLenses ''InfoTable
