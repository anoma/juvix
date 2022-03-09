{-# LANGUAGE StandaloneKindSignatures #-}
module MiniJuvix.Syntax.Concrete.Language.Stage where

import MiniJuvix.Prelude

data Stage
  = Parsed
  | Scoped
  deriving stock (Show)

$(genSingletons [''Stage])
