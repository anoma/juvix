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
    <> [ Suite "fold" (allVariantsExcept [C] [CoreEval]),
         Suite "mapfold" (allVariantsExcept [C] [CoreEval])
       ]

-- "mapfun" -- juvix crashes: Address boundary error
-- "ackermann", -- juvix crashes
-- "combinations", -- juvix crashes:  call stack exhausted
-- "cps", juvix: call stack exhausted
-- "prime" -- juvix: Address boundary error

defaultSuite :: String -> Suite
defaultSuite title =
  Suite
    { _suiteTitle = title,
      _suiteVariants = defaultVariants
    }
