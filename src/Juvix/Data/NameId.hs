module Juvix.Data.NameId where

import Juvix.Prelude.Base
import Prettyprinter

newtype NameId = NameId
  { _unNameId :: Word64
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

makeLenses ''NameId

instance Pretty NameId where
  pretty (NameId w) = pretty w

instance Hashable NameId
