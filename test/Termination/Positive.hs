module Termination.Positive where

import Base
import MiniJuvix.Pipeline
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
            (void . runIO) (upToMicroJuvix entryPoint)
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
            let entryPoint = EntryPoint "." True True (pure _file)
            (void . runIO) (upToMicroJuvix entryPoint)
        }

--------------------------------------------------------------------------------

tests :: [PosTest]
tests =
  [ PosTest "Ackerman nice def. is terminating" "." "Ack.mjuvix",
    PosTest "Recursive functions on Lists" "." "Data/List.mjuvix"
  ]

testsWithKeyword :: [PosTest]
testsWithKeyword =
  [ PosTest "terminating added to fx:=fx" "." "ToEmpty.mjuvix",
    PosTest "terminating for all functions in the mutual block" "." "Mutual.mjuvix",
    PosTest "Undefined is terminating by assumption" "." "Undefined.mjuvix"
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
        "Bypass checking using --non-termination flag on negative tests"
        (map (mkTest . testDescrFlag) negTests),
      testGroup
        "Terminating keyword"
        (map (mkTest . testDescr) testsWithKeyword)
    ]
