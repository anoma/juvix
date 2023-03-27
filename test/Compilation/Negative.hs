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
      "Pattern matching coverage"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
  ]
