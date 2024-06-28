module Juvix.Data.TopModulePathKey where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Juvix.Prelude.Path
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

nonEmptyToTopModulePathKey :: NonEmpty Text -> TopModulePathKey
nonEmptyToTopModulePathKey l =
  TopModulePathKey
    { _modulePathKeyDir = NonEmpty.init l,
      _modulePathKeyName = NonEmpty.last l
    }

relPathtoTopModulePathKey :: Path Rel File -> TopModulePathKey
relPathtoTopModulePathKey =
  nonEmptyToTopModulePathKey
    . fmap pack
    . nonEmpty'
    . splitDirectories
    . toFilePath
    . removeExtensions
