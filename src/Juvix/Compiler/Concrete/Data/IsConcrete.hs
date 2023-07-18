module Juvix.Compiler.Concrete.Data.IsConcrete where

import Juvix.Prelude

data IsConcrete
  = NotConcrete
  | Concrete

$(genSingletons [''IsConcrete])
