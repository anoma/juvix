module Juvix.Syntax.Usage where

import Juvix.Prelude

data Usage
  = UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show, Eq, Ord)
