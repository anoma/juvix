module Juvix.Compiler.Store.Internal.Data.PolarityTable where

import Juvix.Compiler.Internal.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

newtype PolarityTable = PolarityTable
  { -- | The value should have the same length as the length of _inductiveParameters
    _polarityTable :: HashMap InductiveId [Polarity]
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Generic)

instance Serialize PolarityTable

instance NFData PolarityTable

makeLenses ''PolarityTable
