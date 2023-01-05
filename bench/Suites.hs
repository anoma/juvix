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
      "maybe"
    ]
    <> [ Suite suiteName (allVariantsExcept [C] [CoreEval])
         | suiteName <- ["fold", "mapfold"]
       ]
    <> [Suite "mapfun" (allVariantsExcept [C] [CoreEval, JuvixExe, JuvixWasm])]
    <> [ Suite suiteName (allVariantsExcept [] [CoreEval, JuvixExe, JuvixWasm])
         | suiteName <- ["ackermann", "combinations", "cps", "prime"]
       ]

defaultSuite :: String -> Suite
defaultSuite title =
  Suite
    { _suiteTitle = title,
      _suiteVariants = defaultVariants
    }
