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
          _testAssertion = Steps $ compileErrorAssertion file'
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
      $(mkRelFile "test005.juvix")
  ]
