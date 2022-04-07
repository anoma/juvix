module MiniJuvix.Pipeline.Stage where

import MiniJuvix.Prelude

data Pipe =
  Entry
  | Parsing
  | Scoping
  | Abstract
  deriving stock (Eq)

$(genSingletons [''Pipe])
