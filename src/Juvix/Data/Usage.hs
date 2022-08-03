module Juvix.Data.Usage where

import Juvix.Prelude.Base

data Usage
  = UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show, Eq, Ord)
