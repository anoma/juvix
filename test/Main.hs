module Main (main) where

import Base
import Scope qualified
import TypeCheck qualified

negatives :: TestTree
negatives =
  testGroup
    "MiniJuvix tests"
    [ Scope.allTests,
      TypeCheck.allTests
    ]

allTests :: TestTree
allTests = negatives

main :: IO ()
main = defaultMain allTests
