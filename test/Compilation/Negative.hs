module Compilation.Negative where

import Base
import Compilation.Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Compilation/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ compileErrorAssertion tRoot file'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix compilation pipeline negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Test001: Pattern matching coverage"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix"),
    NegTest
      "Test002: Pattern matching coverage in cases"
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix"),
    NegTest
      "Test003: Pattern matching coverage in lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test003.juvix"),
    NegTest
      "Test004: The definition of main has a function type"
      $(mkRelDir ".")
      $(mkRelFile "test004.juvix"),
    NegTest
      "Test005: Axiom"
      $(mkRelDir ".")
      $(mkRelFile "test005.juvix"),
    NegTest
      "Test006: Ill scoped term (This is a bug. It should be positive)"
      $(mkRelDir ".")
      $(mkRelFile "test006.juvix"),
    NegTest
      "Test007: Pattern matching coverage with side conditions"
      $(mkRelDir ".")
      $(mkRelFile "test007.juvix"),
    NegTest
      "Test008: Redundant pattern detection"
      $(mkRelDir ".")
      $(mkRelFile "test008.juvix"),
    NegTest
      "Test009: Redundant pattern detection with side conditions"
      $(mkRelDir ".")
      $(mkRelFile "test009.juvix"),
    NegTest
      "Test010: Redundant pattern detection with complex patterns"
      $(mkRelDir ".")
      $(mkRelFile "test010.juvix"),
    NegTest
      "Test011: Redundant pattern detection with side conditions"
      $(mkRelDir ".")
      $(mkRelFile "test011.juvix"),
    NegTest
      "Test012: Pattern matching coverage with side conditions"
      $(mkRelDir ".")
      $(mkRelFile "test012.juvix"),
    NegTest
      "Test013: Redundant side condition detection"
      $(mkRelDir ".")
      $(mkRelFile "test013.juvix"),
    NegTest
      "Test014: Non-exhaustive left-hand side pattern"
      $(mkRelDir ".")
      $(mkRelFile "test014.juvix")
  ]
