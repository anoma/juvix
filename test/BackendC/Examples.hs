module BackendC.Examples where

import BackendC.Base
import Base
import Data.FileEmbed

data ExampleTest
  = ExampleExecTest ExecTest
  | ExampleCGenOnlyTest TestSpec

data TestSpec = TestSpec
  { _name :: String,
    _relDir :: FilePath,
    _mainFile :: FilePath
  }

data ExecTest = ExecTest
  { _spec :: TestSpec,
    _expectedDir :: FilePath,
    _stdinText :: Text,
    _compileMode :: CompileMode
  }

makeLenses ''ExecTest
makeLenses ''TestSpec

execTest :: String -> FilePath -> FilePath -> FilePath -> Text -> CompileMode -> ExecTest
execTest _name _relDir _mainFile _expectedDir _stdinText _compileMode = ExecTest {_spec = TestSpec {..}, ..}

exampleRoot :: FilePath
exampleRoot = "examples/milestone"

testDescr :: ExampleTest -> TestDescr
testDescr = \case
  ExampleExecTest (ExecTest {..}) ->
    let mainRoot = exampleRoot </> _spec ^. relDir
        expectedFile = $(makeRelativeToProject "tests/examplesExpected" >>= strToExp) </> _expectedDir </> "expected.golden"
     in TestDescr
          { _testName = _spec ^. name,
            _testRoot = mainRoot,
            _testAssertion = case _compileMode of
              WASI stdlibMode -> Steps $ wasiClangAssertion stdlibMode (_spec ^. mainFile) expectedFile _stdinText
              WASM i -> Steps $ wasmClangAssertion i (_spec ^. mainFile) expectedFile
          }
  ExampleCGenOnlyTest (TestSpec {..}) ->
    let mainRoot = exampleRoot </> _relDir
     in TestDescr
          { _testName = _name,
            _testRoot = mainRoot,
            _testAssertion = Steps $ wasmClangAssertionCGenOnly _mainFile
          }

allTests :: TestTree
allTests =
  testGroup
    "Backend C milestone example tests"
    (map (mkTest . testDescr) tests)

tests :: [ExampleTest]
tests =
  [ ExampleExecTest $ execTest "Validity Predicate example" "ValidityPredicates" "Tests.juvix" "ValidityPredicates" "" (WASI StdlibInclude),
    ExampleExecTest $ execTest "TicTacToe CLI example" "TicTacToe" "CLI/TicTacToe.juvix" "TicTacToe" "aaa\n0\n10\n1\n2\n3\n3\n4\n5\n6\n7\n8\n9\n" (WASI StdlibInclude),
    ExampleCGenOnlyTest $ TestSpec "TicTacToe Web example (C gen only)" "TicTacToe" "Web/TicTacToe.juvix",
    ExampleExecTest $ execTest "Fibonacci example" "Fibonacci" "Fibonacci.juvix" "Fibonacci" "" (WASI StdlibInclude),
    ExampleExecTest $ execTest "Collatz sequence generator" "Collatz" "Collatz.juvix" "Collatz" "123\n" (WASI StdlibInclude),
    ExampleExecTest $ execTest "Towers of Hanoi" "Hanoi" "Hanoi.juvix" "Hanoi" "" (WASI StdlibInclude),
    ExampleExecTest $ execTest "Pascal's triangle" "PascalsTriangle" "PascalsTriangle.juvix" "PascalsTriangle" "" (WASI StdlibInclude)
  ]
