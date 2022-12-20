module Termination.Positive where

import Base
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Pipeline
import Termination.Negative qualified as N

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive/Termination")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = (defaultEntryPoint tRoot file') {_entryPointNoStdlib = True}
            (void . runIO' iniState entryPoint) upToInternal
        }

--------------------------------------------------------------------------------
-- Testing --no-termination flag with all termination negative tests
--------------------------------------------------------------------------------

rootNegTests :: Path Abs Dir
rootNegTests = relToProject $(mkRelDir "tests/negative/Termination")

testDescrFlag :: N.NegTest -> TestDescr
testDescrFlag N.NegTest {..} =
  let tRoot = rootNegTests <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint =
                  (defaultEntryPoint tRoot file')
                    { _entryPointNoTermination = True,
                      _entryPointNoStdlib = True
                    }

            (void . runIO' iniState entryPoint) upToInternal
        }

--------------------------------------------------------------------------------

tests :: [PosTest]
tests =
  [ PosTest
      "Ackerman nice def. is terminating"
      $(mkRelDir ".")
      $(mkRelFile "Ack.juvix"),
    PosTest
      "Fibonacci with nested pattern"
      $(mkRelDir ".")
      $(mkRelFile "Fib.juvix"),
    PosTest
      "Recursive functions on Lists"
      $(mkRelDir ".")
      $(mkRelFile "Data/List.juvix")
  ]

testsWithKeyword :: [PosTest]
testsWithKeyword =
  [ PosTest
      "terminating added to fx:=fx"
      $(mkRelDir ".")
      $(mkRelFile "ToEmpty.juvix"),
    PosTest
      "terminating for all functions in the mutual block"
      $(mkRelDir ".")
      $(mkRelFile "Mutual.juvix"),
    PosTest
      "Undefined is terminating by assumption"
      $(mkRelDir ".")
      $(mkRelFile "Undefined.juvix")
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
