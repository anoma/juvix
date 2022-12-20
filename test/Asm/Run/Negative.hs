module Asm.Run.Negative where

import Asm.Run.Base
import Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Asm/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunErrorAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixAsm run negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Division by zero"
      $(mkRelDir ".")
      $(mkRelFile "test001.jva"),
    NegTest
      "Invalid memory access"
      $(mkRelDir ".")
      $(mkRelFile "test002.jva"),
    NegTest
      "No matching case branch"
      $(mkRelDir ".")
      $(mkRelFile "test003.jva")
  ]
