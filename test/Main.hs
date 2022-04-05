module Main (main) where

import Base
import Scope qualified
import TypeCheck qualified

allTests :: TestTree
allTests =
  testGroup
    "MiniJuvix tests"
    [ Scope.allTests,
      TypeCheck.allTests
    ]

main :: IO ()
main = defaultMain allTests
