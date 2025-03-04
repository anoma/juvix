module Casm.Compilation.Negative where

import Base
import Casm.Compilation.Base

data NegTest = NegTest
  { _name :: String,
    _runVM :: Bool,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Casm/Compilation/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ compileErrorAssertion _runVM tRoot file'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix to CASM negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Test001: Wrong `main` argument type"
      False
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix"),
    NegTest
      "Test002: Wrong `main` result type"
      False
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix"),
    NegTest
      "Test003: Range check failure"
      True
      $(mkRelDir ".")
      $(mkRelFile "test003.juvix")
  ]
