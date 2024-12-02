module Juvix.Data.PackageId where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude.Base

data PackageId = PackageId
  { _packageIdName :: Text,
    _packageIdVersion :: SemVer
  }
  deriving stock (Show, Ord, Eq, Data, Generic)

makeLenses ''PackageId

packageBaseId :: PackageId
packageBaseId =
  PackageId
    { _packageIdName = Str.packageBase,
      _packageIdVersion = defaultVersion
    }

defaultVersion :: SemVer
defaultVersion = SemVer 0 0 0 Nothing Nothing

instance Serialize PackageId

instance Hashable PackageId

instance NFData PackageId
