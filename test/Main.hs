module Main (main) where

import Arity qualified
import Asm qualified
import BackendGeb qualified
import BackendMarkdown qualified
import Base
import Compilation qualified
import Core qualified
import Examples qualified
import Format qualified
import Formatter qualified
import Internal qualified
import Package qualified
import Parsing qualified
import Reachability qualified
import Runtime qualified
import Scope qualified
import Termination qualified
import Typecheck qualified
import VampIR qualified
import Lock qualified
import Control.Concurrent.Extra (newLock)

slowTests :: TestTree
slowTests =
  testGroup
    "Juvix slow tests"
    [ BackendGeb.allTests,
      Runtime.allTests,
      Asm.allTests,
      Core.allTests,
      Internal.allTests,
      Compilation.allTests,
      Examples.allTests,
      VampIR.allTests
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
      Format.allTests,
      Formatter.allTests,
      Package.allTests,
      BackendMarkdown.allTests
    ]

main :: IO ()
main = do
  l <- newLock
  defaultMain (testGroup "Juvix tests" [fastTests, after AllFinish "Juvix fast tests" slowTests, Lock.allTests l])
