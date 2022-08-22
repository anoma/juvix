module Main (main) where

import Arity qualified
import BackendC qualified
import Base
import Core qualified
import MonoJuvix qualified
import Reachability qualified
import Scope qualified
import Termination qualified
import TypeCheck qualified

slowTests :: TestTree
slowTests =
  testGroup
    "Juvix slow tests"
    [ BackendC.allTests,
      Core.allTests
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Juvix fast tests"
    [ Scope.allTests,
      Termination.allTests,
      Arity.allTests,
      TypeCheck.allTests,
      Reachability.allTests,
      MonoJuvix.allTests
    ]

main :: IO ()
main = do
  defaultMain (testGroup "Juvix tests" [fastTests, slowTests])
