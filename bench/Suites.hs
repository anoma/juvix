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
         Suite "mapfold" (allVariantsExcept [C] [CoreEval]),
         Suite "mapfun" (allVariantsExcept [C] [CoreEval])
       ]

-- "ackermann", -- juvix crashes
-- "combinations", -- juvix crashes:  call stack exhausted
-- "cps", call stack exhausted
-- "prime" -- Address boundary error

defaultSuite :: String -> Suite
defaultSuite title =
  Suite
    { _suiteTitle = title,
      _suiteVariants = defaultVariants
    }
