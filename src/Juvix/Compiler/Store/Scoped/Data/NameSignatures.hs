module Juvix.Compiler.Store.Scoped.Data.NameSignatures where

import Juvix.Compiler.Concrete.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

newtype NameSignatures = NameSignatures
  { _nameSignatures :: HashMap NameId (NameSignature 'Scoped)
  }
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

instance Serialize NameSignatures

newtype ConstructorNameSignatures = ConstructorNameSignatures
  { _constructorNameSignatures :: HashMap NameId (RecordNameSignature 'Scoped)
  }
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

instance Serialize ConstructorNameSignatures

makeLenses ''NameSignatures
makeLenses ''ConstructorNameSignatures
