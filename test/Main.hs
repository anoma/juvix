module Main (main) where

import Anoma qualified
import Asm qualified
import BackendGeb qualified
import BackendMarkdown qualified
import Base
import Casm qualified
import Compilation qualified
import Core qualified
import Examples qualified
import Format qualified
import Formatter qualified
import Internal qualified
import Nockma qualified
import Package qualified
import Parsing qualified
import Reg qualified
import Repl qualified
import Resolver qualified
import Runtime qualified
import Rust qualified
import Scope qualified
import Termination qualified
import Tree qualified
import Typecheck qualified
import VampIR qualified

slowTests :: TestTree
slowTests =
--  sequentialTestGroup
    testGroup
    "Juvix slow tests"
--    AllFinish
    [ BackendGeb.allTests,
      Runtime.allTests,
      Reg.allTests,
      Asm.allTests,
      Tree.allTests,
      Core.allTests,
      Internal.allTests,
      Compilation.allTests,
      Examples.allTests,
      Rust.allTests,
      Casm.allTests,
      VampIR.allTests,
      Anoma.allTests,
      Repl.allTests
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Juvix fast tests"
    [ Parsing.allTests,
      Resolver.allTests,
      Scope.allTests,
      Termination.allTests,
      Typecheck.allTests,
      Format.allTests,
      Formatter.allTests,
      Package.allTests,
      BackendMarkdown.allTests,
      Nockma.allTests
    ]

main :: IO ()
main = defaultMain (testGroup "Juvix tests" [fastTests, slowTests])
