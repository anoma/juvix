module BackendC.Examples where

import BackendC.Base
import Base
import Data.FileEmbed

data ExampleTest
  = ExampleExecTest ExecTest
  | ExampleCGenOnlyTest TestSpec

data TestSpec = TestSpec
  { _name :: String,
    _testDir :: Path Rel Dir,
    _mainFile :: Path Rel File
  }

data ExecTest = ExecTest
  { _spec :: TestSpec,
    _expectedDir :: Path Rel Dir,
    _stdinText :: Text,
    _compileMode :: CompileMode
  }

makeLenses ''ExecTest
makeLenses ''TestSpec

execTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel Dir -> Text -> CompileMode -> ExecTest
execTest _name _testDir _mainFile _expectedDir _stdinText _compileMode = ExecTest {_spec = TestSpec {..}, ..}

exampleRoot :: Path Abs Dir
exampleRoot = absDir $(makeRelativeToProject (toFilePath $(mkRelDir "examples/milestone")) >>= strToExp)

testDescr :: ExampleTest -> TestDescr
testDescr = \case
  ExampleExecTest ExecTest {..} ->
    let mainRoot = exampleRoot <//> _spec ^. testDir
        mainFile' = mainRoot <//> (_spec ^. mainFile)
        expectedFile = absDir $(makeRelativeToProject "tests/examplesExpected" >>= strToExp) <//> _expectedDir <//> $(mkRelFile "expected.golden")
     in TestDescr
          { _testName = _spec ^. name,
            _testRoot = mainRoot,
            _testAssertion = case _compileMode of
              WASI stdlibMode -> Steps $ wasiClangAssertion stdlibMode mainFile' expectedFile _stdinText
              WASM i -> Steps $ wasmClangAssertion i mainFile' expectedFile
          }
  ExampleCGenOnlyTest TestSpec {..} ->
    let mainRoot = exampleRoot <//> _testDir
        mainFile' = mainRoot <//> _mainFile
     in TestDescr
          { _testName = _name,
            _testRoot = mainRoot,
            _testAssertion = Steps $ wasmClangAssertionCGenOnly mainFile'
          }

allTests :: TestTree
allTests =
  testGroup
    "Backend C milestone example tests"
    (map (mkTest . testDescr) tests)

tests :: [ExampleTest]
tests =
  map ExampleExecTest execTests <> map ExampleCGenOnlyTest testSpecs
  where
    execTests :: [ExecTest]
    execTests =
      [ execTest "Validity Predicate example" $(mkRelDir "ValidityPredicates") $(mkRelFile "Tests.juvix") $(mkRelDir "ValidityPredicates") "" (WASI StdlibInclude),
        execTest "TicTacToe CLI example" $(mkRelDir "TicTacToe") $(mkRelFile "CLI/TicTacToe.juvix") $(mkRelDir "TicTacToe") "aaa\n0\n10\n1\n2\n3\n3\n4\n5\n6\n7\n8\n9\n" (WASI StdlibInclude),
        execTest "Fibonacci example" $(mkRelDir "Fibonacci") $(mkRelFile "Fibonacci.juvix") $(mkRelDir "Fibonacci") "" (WASI StdlibInclude),
        execTest "Collatz sequence generator" $(mkRelDir "Collatz") $(mkRelFile "Collatz.juvix") $(mkRelDir "Collatz") "123\n" (WASI StdlibInclude),
        execTest "Towers of Hanoi" $(mkRelDir "Hanoi") $(mkRelFile "Hanoi.juvix") $(mkRelDir "Hanoi") "" (WASI StdlibInclude),
        execTest "Pascal's triangle" $(mkRelDir "PascalsTriangle") $(mkRelFile "PascalsTriangle.juvix") $(mkRelDir "PascalsTriangle") "" (WASI StdlibInclude)
      ]
    testSpecs :: [TestSpec]
    testSpecs = [TestSpec "TicTacToe Web example (C gen only)" $(mkRelDir "TicTacToe") $(mkRelFile "Web/TicTacToe.juvix")]
