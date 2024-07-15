module Main (main) where

import Anoma qualified
import Asm qualified
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

slowTests :: IO TestTree
slowTests =
  sequentialTestGroup
    "Juvix slow tests"
    AllFinish
    <$> sequence
      [ return Runtime.allTests,
        return Reg.allTests,
        return Asm.allTests,
        return Tree.allTests,
        return Core.allTests,
        return Internal.allTests,
        return Compilation.allTests,
        return Examples.allTests,
        Rust.allTests,
        Casm.allTests,
        VampIR.allTests,
        return Anoma.allTests,
        return Repl.allTests
      ]

fastTests :: IO TestTree
fastTests =
  return
    $ testGroup
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
main = do
  tests <- sequence [fastTests, slowTests]
  defaultMain (testGroup "Juvix tests" tests)
