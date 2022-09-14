module Termination.Positive where

import Base
import Juvix.Compiler.Pipeline
import Termination.Negative qualified as N

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath
  }

root :: FilePath
root = "tests/positive/Termination"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = (defaultEntryPoint _file) {_entryPointNoStdlib = True}
            (void . runIO' entryPoint) upToInternal
        }

--------------------------------------------------------------------------------
-- Testing --no-termination flag with all termination negative tests
--------------------------------------------------------------------------------

rootNegTests :: FilePath
rootNegTests = "tests/negative/Termination"

testDescrFlag :: N.NegTest -> TestDescr
testDescrFlag N.NegTest {..} =
  let tRoot = rootNegTests </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint =
                  (defaultEntryPoint _file)
                    { _entryPointRoot = ".",
                      _entryPointNoTermination = True,
                      _entryPointNoStdlib = True
                    }

            (void . runIO' entryPoint) upToInternal
        }

--------------------------------------------------------------------------------

tests :: [PosTest]
tests =
  [ PosTest "Ackerman nice def. is terminating" "." "Ack.juvix",
    PosTest "Fibonacci with nested pattern" "." "Fib.juvix",
    PosTest "Recursive functions on Lists" "." "Data/List.juvix"
  ]

testsWithKeyword :: [PosTest]
testsWithKeyword =
  [ PosTest "terminating added to fx:=fx" "." "ToEmpty.juvix",
    PosTest "terminating for all functions in the mutual block" "." "Mutual.juvix",
    PosTest "Undefined is terminating by assumption" "." "Undefined.juvix"
  ]

negTests :: [N.NegTest]
negTests = N.tests

--------------------------------------------------------------------------------

allTests :: TestTree
allTests =
  testGroup
    "Positive tests"
    [ testGroup
        "Well-known terminating functions"
        (map (mkTest . testDescr) tests),
      testGroup
        "Bypass termination checking using --non-termination flag on negative tests"
        (map (mkTest . testDescrFlag) negTests),
      testGroup
        "Terminating keyword"
        (map (mkTest . testDescr) testsWithKeyword)
    ]
