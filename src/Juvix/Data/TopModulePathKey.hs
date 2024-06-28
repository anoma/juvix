module Juvix.Data.TopModulePathKey where

import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty as Pretty

data TopModulePathKey = TopModulePathKey
  { _modulePathKeyDir :: [Text],
    _modulePathKeyName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize TopModulePathKey

instance NFData TopModulePathKey

instance Hashable TopModulePathKey

makeLenses ''TopModulePathKey

instance Pretty TopModulePathKey where
  pretty (TopModulePathKey path name) =
    mconcat (punctuate Pretty.dot (map pretty (snoc path name)))
