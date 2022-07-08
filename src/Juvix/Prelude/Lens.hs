module Juvix.Prelude.Lens where

import Juvix.Prelude.Base

-- | points to the first element of a non-empty list.
_head :: Lens' (NonEmpty a) a
_head = singular each
