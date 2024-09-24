module Juvix.Data.Polarity where

import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data Polarity
  = -- TODO rename PolarityNegative to PolarityNotStrictlyPositive
    PolarityNegative
  | PolarityStrictlyPositive
  | PolarityUnused
  deriving stock (Eq, Generic, Data, Show)

instance Hashable Polarity

instance Serialize Polarity

instance NFData Polarity

instance Semigroup Polarity where
  a <> b = case (a, b) of
    (PolarityUnused, p) -> p
    (p, PolarityUnused) -> p
    (PolarityNegative, _) -> PolarityNegative
    (_, PolarityNegative) -> PolarityNegative
    (PolarityStrictlyPositive, PolarityStrictlyPositive) -> PolarityStrictlyPositive

instance Monoid Polarity where
  mempty = PolarityUnused

instance Pretty Polarity where
  pretty = \case
    PolarityNegative -> "negative"
    PolarityStrictlyPositive -> "strictly-positive"
    PolarityUnused -> "unused"
