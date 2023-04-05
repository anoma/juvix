module Golden where

import Mari.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

absurdTestAll :: T.TestTree
absurdTestAll =
  T.testGroup
    "Two Failing test groups"
    [ T.testCase "(2 T.@=? 3)" ((2 :: Integer) T.@=? 3),
      T.testCase "(4 T.@=? 3)" ((4 :: Integer) T.@=? 3)
    ]

absurdTestSome :: T.TestTree
absurdTestSome =
  T.testGroup
    "One succeeding, one failing test group"
    [ T.testCase "(2 T.@=? 2)" ((2 :: Integer) T.@=? 2),
      T.testCase "(4 T.@=? 3)" ((4 :: Integer) T.@=? 3)
    ]
