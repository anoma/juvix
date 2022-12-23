module Suites where

import Base
import Juvix.Prelude
import Variants

suites :: [Suite]
suites =
  map
    defaultSuite
    [ "mergesort",
      "fibonacci",
      "ackermann", -- juvix crashes
      "combinations", -- juvix crashes: out of call stack
      "cps",
      "fold",
      "mapfold",
      "mapfun",
      "maybe",
      "prime"
    ]

defaultSuite :: String -> Suite
defaultSuite title =
  Suite
    { _suiteTitle = title,
      _suiteVariants = defaultVariants
    }
