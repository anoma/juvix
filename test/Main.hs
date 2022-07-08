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
    "Juvix slow tests"
    [BackendC.allTests]

fastTests :: TestTree
fastTests =
  testGroup
    "Juvix fast tests"
    [ Scope.allTests,
      Termination.allTests,
      Arity.allTests,
      TypeCheck.allTests,
      MonoJuvix.allTests
    ]

main :: IO ()
main = do
  defaultMain (testGroup "Juvix tests" [fastTests, slowTests])
