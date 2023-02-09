module Geb.Eval.Negative where

import Base
import Geb.Eval.Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Geb/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreEvalErrorAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Division by zero"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvc"),
    NegTest
      "Arithmetic operations on non-numbers"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvc"),
    NegTest
      "Matching on non-data"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvc"),
    NegTest
      "If on non-boolean"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvc"),
    NegTest
      "No matching case branch"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvc"),
    NegTest
      "Invalid application"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvc"),
    NegTest
      "Invalid builtin application"
      $(mkRelDir ".")
      $(mkRelFile "test007.jvc"),
    NegTest
      "Undefined symbol"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvc"),
    NegTest
      "Erroneous Church numerals"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvc"),
    NegTest
      "Empty letrec"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvc")
  ]
