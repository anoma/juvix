module Suites where

import Base
import Juvix.Prelude
import Variants

suites :: [Suite]
suites =
  map
    defaultSuite
    [ "mergesort",
      "fibonacci"
    ]

-- "ackermann"
-- "combinations"
-- "cps"
-- "fibonacci"
-- "fold"
-- "mapfold"
-- "mapfun"
-- "maybe"
-- "mergesort"
-- "prime

defaultSuite :: String -> Suite
defaultSuite title =
  Suite
    { _suiteTitle = title,
      _suiteVariants = defaultVariants
    }
