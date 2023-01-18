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

-- instance Semigroup InfoTable where
--   (InfoTable a b) <> (InfoTable a' b') =
--     InfoTable
--       { _infoParsedItems = a <> a',
--         _infoParsedComments = b <> b'
--       }

-- instance Monoid InfoTable where
--   mempty =
--     InfoTable
--       { _infoParsedItems = mempty,
--         _infoParsedComments = mempty
--       }
