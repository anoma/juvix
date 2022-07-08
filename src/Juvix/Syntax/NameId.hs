module Juvix.Syntax.NameId where

import Juvix.Prelude
import Prettyprinter

newtype NameId = NameId
  { _unNameId :: Word64
  }
  deriving stock (Show, Eq, Ord, Generic)

makeLenses ''NameId

instance Pretty NameId where
  pretty (NameId w) = pretty w

instance Hashable NameId
