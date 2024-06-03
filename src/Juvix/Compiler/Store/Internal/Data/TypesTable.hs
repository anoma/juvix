module Juvix.Compiler.Store.Internal.Data.TypesTable where

import Juvix.Compiler.Internal.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

newtype TypesTable = TypesTable
  { _typesTable :: HashMap NameId Expression
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Generic)

instance Serialize TypesTable

instance NFData TypesTable

makeLenses ''TypesTable
