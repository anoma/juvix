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
      "combinations",
      "maybe",
      "ackermann",
      "cps",
      "prime"
    ]
    <> [ Suite suiteName (allVariantsExcept [C] [CoreEval])
         | suiteName <- ["fold", "mapfold", "mapfun"]
       ]

defaultSuite :: String -> Suite
defaultSuite title =
  Suite
    { _suiteTitle = title,
      _suiteVariants = defaultVariants
    }
