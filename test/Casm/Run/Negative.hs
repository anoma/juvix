module Casm.Run.Negative where

import Base
import Casm.Run.Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Casm/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ casmRunErrorAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "CASM run negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Test001: Undefined label"
      $(mkRelDir ".")
      $(mkRelFile "test001.casm"),
    NegTest
      "Test002: Invalid memory access"
      $(mkRelDir ".")
      $(mkRelFile "test002.casm"),
    NegTest
      "Test003: Non-continuous memory use"
      $(mkRelDir ".")
      $(mkRelFile "test003.casm"),
    NegTest
      "Test004: Double memory write"
      $(mkRelDir ".")
      $(mkRelFile "test004.casm"),
    NegTest
      "Test005: Invalid register use"
      $(mkRelDir ".")
      $(mkRelFile "test005.casm")
  ]
