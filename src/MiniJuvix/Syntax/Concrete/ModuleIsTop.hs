{-# LANGUAGE StandaloneKindSignatures #-}

module MiniJuvix.Syntax.Concrete.ModuleIsTop where

import MiniJuvix.Prelude

data ModuleIsTop = ModuleTop | ModuleLocal
  deriving stock (Eq, Ord, Show)

$(genSingletons [''ModuleIsTop])
