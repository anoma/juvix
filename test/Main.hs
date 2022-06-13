module Main (main) where

import Arity qualified
import BackendC qualified
import Base
import MonoJuvix qualified
import Scope qualified
import Termination qualified
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
      Termination.allTests,
      Arity.allTests,
      TypeCheck.allTests,
      MonoJuvix.allTests
    ]

main :: IO ()
main = do
  defaultMain (testGroup "MiniJuvix tests" [fastTests, slowTests])
