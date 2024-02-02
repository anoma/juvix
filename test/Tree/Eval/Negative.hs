module Tree.Eval.Negative where

import Base
import Tree.Eval.Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Tree/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeEvalErrorAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixTree negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Test001: Division by zero"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvt"),
    NegTest
      "Test002: Arithmetic operations on non-numbers"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvt"),
    NegTest
      "Test003: Case on non-data"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvt"),
    NegTest
      "Test004: If on non-boolean"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvt"),
    NegTest
      "Test005: No matching case branch"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvt"),
    NegTest
      "Test006: Invalid closure call"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvt"),
    NegTest
      "Test007: Call: wrong number of arguments"
      $(mkRelDir ".")
      $(mkRelFile "test007.jvt"),
    NegTest
      "Test008: Closure call: wrong number of arguments"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvt")
  ]
