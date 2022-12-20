module Juvix.Data.Processed where

import Juvix.Prelude.Base

data IsProcessed
  = Raw
  | Processed
  deriving stock (Eq, Show)

$(genSingletons [''IsProcessed])
