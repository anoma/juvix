module VampIR.Compilation.Negative where

import Base
import VampIR.Compilation.Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VampIR/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ vampirCompileErrorAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix to VampIR compilation negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Test001: Missing comma"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
  ]
