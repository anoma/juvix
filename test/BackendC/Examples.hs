module BackendC.Examples where

import BackendC.Base
import Base
import Data.FileEmbed

data ExampleTest = ExampleTest
  { _name :: String,
    _relDir :: FilePath,
    _mainFile :: FilePath,
    _expectedDir :: FilePath,
    _stdinText :: Text
  }

makeLenses ''ExampleTest

exampleRoot :: FilePath
exampleRoot = "examples/milestone"

testDescr :: ExampleTest -> TestDescr
testDescr ExampleTest {..} =
  let mainRoot = exampleRoot </> _relDir
      expectedFile = $(makeRelativeToProject "tests/examplesExpected" >>= strToExp) </> _expectedDir </> "expected.golden"
   in TestDescr
        { _testName = _name,
          _testRoot = mainRoot,
          _testAssertion = Steps $ clangAssertion StdlibExclude _mainFile expectedFile _stdinText
        }

allTests :: TestTree
allTests =
  testGroup
    "Backend C milestone example tests"
    (map (mkTest . testDescr) tests)

tests :: [ExampleTest]
tests =
  [ ExampleTest "Validity Predicate example" "ValidityPredicates" "Tests.mjuvix" "ValidityPredicates" "",
    ExampleTest "MiniTicTacToe example" "MiniTicTacToe" "MiniTicTacToe.mjuvix" "MiniTicTacToe" "aaa\n0\n10\n1\n2\n3\n3\n4\n5\n6\n7\n8\n9\n"
  ]
