module Main (main) where

import BackendC qualified
import Base
import MonoJuvix qualified
import Scope qualified
import TypeCheck qualified

slowTests :: TestTree
slowTests =
  testGroup
    "MiniJuvix slow tests"
    [BackendC.allTests]

fastTests :: TestTree
fastTests =
  testGroup
    "MiniJuvix fast tests"
    [ Scope.allTests,
      TypeCheck.allTests,
      MonoJuvix.allTests
    ]

main :: IO ()
main = do
  defaultMain (testGroup "MiniJuvix tests" [fastTests, slowTests])
