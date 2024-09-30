module Juvix.Data.Polarity where

import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

-- | NOTE that Order of constructors is relevant for the derived instances
data Polarity
  = PolarityUnused
  | PolarityStrictlyPositive
  | PolarityNonStrictlyPositive
  deriving stock (Ord, Enum, Bounded, Eq, Generic, Data, Show)

instance Hashable Polarity

instance Serialize Polarity

instance NFData Polarity

instance Semigroup Polarity where
  a <> b = max a b

instance Monoid Polarity where
  mempty = PolarityUnused

instance Pretty Polarity where
  pretty = \case
    PolarityNonStrictlyPositive -> "non-strictly-positive"
    PolarityStrictlyPositive -> "strictly-positive"
    PolarityUnused -> "unused"
