module Core.Negative where

import Base
import Core.Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath
  }

root :: FilePath
root = "tests/Core/negative"

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreEvalErrorAssertion _file
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
      "."
      "test001.jvc",
    NegTest
      "Arithmetic operations on non-numbers"
      "."
      "test002.jvc",
    NegTest
      "Matching on non-data"
      "."
      "test003.jvc",
    NegTest
      "If on non-boolean"
      "."
      "test004.jvc",
    NegTest
      "No matching case branch"
      "."
      "test005.jvc",
    NegTest
      "Invalid application"
      "."
      "test006.jvc",
    NegTest
      "Invalid builtin application"
      "."
      "test007.jvc",
    NegTest
      "Undefined symbol"
      "."
      "test008.jvc",
    NegTest
      "Erroneous Chruch numerals"
      "."
      "test009.jvc"
  ]
