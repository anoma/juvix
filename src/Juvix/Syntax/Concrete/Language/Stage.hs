module Juvix.Syntax.Concrete.Language.Stage where

import Juvix.Prelude

data Stage
  = Parsed
  | Scoped
  deriving stock (Eq, Show)

$(genSingletons [''Stage])
