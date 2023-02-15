module BackendGeb.Eval.Negative where

import BackendGeb.Eval.Base
import Base

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
          _testAssertion = Steps $ gebEvalErrorAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixGeb negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Test004.lisp"
      $(mkRelDir ".")
      $(mkRelFile "test004.lisp"),
    NegTest
      "Test006.lisp"
      $(mkRelDir ".")
      $(mkRelFile "test006.lisp")
  ]
