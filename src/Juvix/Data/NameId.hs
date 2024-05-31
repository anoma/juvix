module Juvix.Data.NameId
  ( module Juvix.Data.NameId,
    module Juvix.Data.ModuleId,
  )
where

import Juvix.Data.ModuleId
import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Prettyprinter

data NameId = NameId
  { _nameIdUid :: Word64,
    _nameIdModuleId :: ModuleId
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

makeLenses ''NameId

instance Pretty NameId where
  pretty (NameId w m) = pretty m <> ":" <> pretty w

instance Hashable NameId

instance Serialize NameId

instance NFData NameId
