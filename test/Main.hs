module Main (main) where

import Base
import MonoJuvix qualified
import Scope qualified
import TypeCheck qualified

allTests :: TestTree
allTests =
  testGroup
    "MiniJuvix tests"
    [ Scope.allTests,
      TypeCheck.allTests,
      MonoJuvix.allTests
    ]

main :: IO ()
main = defaultMain allTests
