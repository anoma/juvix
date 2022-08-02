module Juvix.Compiler.Concrete.Data.PublicAnn where

import Juvix.Prelude

data PublicAnn
  = -- | Explicit public annotation
    Public
  | -- | No annotation. Do not confuse this with 'not public' or 'private'.
    NoPublic
  deriving stock (Show, Eq, Ord)
