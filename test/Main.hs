module Main (main) where

import Arity qualified
import Asm qualified
import BackendC qualified
import Base
import Compilation qualified
import Core qualified
import Format qualified
import Internal qualified
import Parsing qualified
import Reachability qualified
import Runtime qualified
import Scope qualified
import Termination qualified
import Typecheck qualified

slowTests :: TestTree
slowTests =
  testGroup
    "Juvix slow tests"
    [ BackendC.allTests,
      Runtime.allTests,
      Asm.allTests,
      Core.allTests,
      Internal.allTests,
      Compilation.allTests
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Juvix fast tests"
    [ Parsing.allTests,
      Scope.allTests,
      Termination.allTests,
      Arity.allTests,
      Typecheck.allTests,
      Reachability.allTests,
      Format.allTests
    ]

main :: IO ()
main = do
  defaultMain (testGroup "Juvix tests" [fastTests, slowTests])
