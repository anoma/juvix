module MiniJuvix.Syntax.Usage where

import MiniJuvix.Prelude

data Usage
  = UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show, Eq, Ord)
